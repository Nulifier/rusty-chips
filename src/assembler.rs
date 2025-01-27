use crate::error::{ChipError, Result};
use multimap::MultiMap;
use scanner::{Instruction, LabelRefDirection, Scanner, Token, TokenType};
use std::iter::Peekable;
use std::{collections::HashMap, rc::Rc};

pub mod position;
pub mod scanner;

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct SectionName(pub Rc<str>);
#[derive(Debug, Clone, Copy)]
pub struct SectionIndex(pub usize);
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct SymbolName(pub Rc<str>);

impl SymbolName {
	pub fn is_global(&self) -> bool {
		!self.0.starts_with('_')
	}
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct NumericSymbol(pub u8);
#[derive(Debug, Clone, Copy)]
struct SectionOffset(pub usize);

struct NumericLabelListEntry {
	entry_type: NumericLabelListEntryType,
	symbol: NumericSymbol,
	section_index: SectionIndex,
	section_offset: SectionOffset,
}

#[derive(PartialEq)]
enum NumericLabelListEntryType {
	Definition,
	ReferenceForward,
	ReferenceBackward,
}

/// Represents a section of the program
pub struct Section {
	/// Section name
	name: SectionName,
	/// Section data
	data: Vec<u8>,
	/// Map of symbol names to their offsets in the section
	symbol_defs: HashMap<SymbolName, SectionOffset>,
	/// Symbol references that need to be updated once the code is generated
	symbol_refs: MultiMap<SymbolName, SectionOffset>,
}

impl Section {
	pub fn new(name: SectionName) -> Self {
		Self {
			name,
			data: Vec::new(),
			symbol_defs: HashMap::new(),
			symbol_refs: MultiMap::new(),
		}
	}
}

const CHECK_OPCODE_MASK: bool = true;

/**
 * Assert that the opcode has no bits outside the mask.
 * These bits are reserved for the opcode arguments.
 */
fn assert_opcode_mask(opcode: u16, mask: u16) {
	if CHECK_OPCODE_MASK {
		let opcode_mask = opcode & (!mask);
		assert_eq!(
			opcode_mask, 0,
			"Bits located outside the mask are set: {:#X}",
			opcode_mask
		);
	}
}

pub struct Assembler<'a> {
	/// Scanner of the source file
	//scanner: Scanner<'a>,
	token_itr: Peekable<Scanner<'a>>,

	/// Sections defined in the source file
	sections: Vec<Section>,

	current_section: SectionIndex,

	/// Index of the section that the symbols are defined in
	global_symbols: HashMap<SymbolName, SectionIndex>,

	/// List of Numeric symbol definitions and references that need to be
	/// updated once the code is generated.
	numeric_label_list: Vec<NumericLabelListEntry>,
}

impl<'a> Assembler<'a> {
	pub fn new(filename: Rc<str>, source: &'a str) -> Self {
		let scanner = Scanner::new(filename, source);
		Self {
			token_itr: scanner.into_iter().peekable(),
			sections: Vec::from([Section::new(SectionName(Rc::from("text")))]),
			current_section: SectionIndex(0),
			global_symbols: HashMap::new(),
			numeric_label_list: Vec::new(),
		}
	}

	pub fn compile(&mut self) -> Result<()> {
		// Read tokens until we reach the end of the file
		while let Some(token) = self.token_itr.next() {
			// Look for a statement
			match token.token_type() {
				TokenType::Comment => continue,   // Skip comments
				TokenType::Semicolon => continue, // Empty statement
				TokenType::Newline => continue,   // Empty statement
				TokenType::Identifier => self.label_def(&token)?,
				TokenType::Number(_, _) => self.numeric_label_def(&token)?,
				TokenType::EoF => break,

				TokenType::Instruction(inst) => self.instruction(&token, inst)?,

				_ => unreachable!("Unexpected token {:?}", token.token_type()),
			}
		}

		Ok(())
	}

	pub fn get_section_index(&self, name: &str) -> Option<SectionIndex> {
		self.sections
			.iter()
			.enumerate()
			.find(|(_, section)| name == &*section.name.0)
			.map(|(index, _)| SectionIndex(index))
	}

	pub fn get_section(&self, index: SectionIndex) -> &Section {
		&self.sections[index.0]
	}

	/**
	 * Provides a concrete start location for a section.
	 * This is used to update the symbol references with a concrete value.
	 */
	pub fn locate_section(
		&mut self,
		section_starts: Vec<u16>,
		global_symbols: HashMap<SymbolName, u16>,
	) -> Result<()> {
		for (i, this_section_start) in section_starts.iter().enumerate() {
			let section = &mut self.sections[i];

			// Update each identifier symbol defined in this section
			for (symbol, references) in section.symbol_refs.iter_all() {
				// Lookup the concrete address of the symbol
				let addr = if symbol.is_global() {
					// Lookup the symbol in the global symbol table
					// Address is the global address
					*global_symbols
						.get(symbol)
						.ok_or(ChipError::UndefinedSymbol(symbol.0.to_string()))?
				} else {
					// Lookup the symbol in the current section
					// Address is the section offset + the section start
					section
						.symbol_defs
						.get(symbol)
						.ok_or(ChipError::UndefinedSymbol(symbol.0.to_string()))?
						.0 as u16 + this_section_start
				};

				// Update each reference with the concrete address
				for offset in references {
					let offset = offset.0;
					let [hi, lo] = addr.to_be_bytes();
					section.data[offset] |= hi;
					section.data[offset + 1] |= lo;
				}
			}
		}

		// Update each numeric label reference
		for (index, entry) in self.numeric_label_list.iter().enumerate() {
			// Find the address of the target label
			let addr = match entry.entry_type {
				// Skip definitions
				NumericLabelListEntryType::Definition => continue,
				NumericLabelListEntryType::ReferenceForward => {
					let (section_index, section_offset) =
						self.find_numeric_label_def_forward(entry.symbol, index)?;
					section_offset.0 as u16 + section_starts[section_index.0]
				}
				NumericLabelListEntryType::ReferenceBackward => {
					let (section_index, section_offset) =
						self.find_numeric_label_def_backward(entry.symbol, index)?;
					section_offset.0 as u16 + section_starts[section_index.0]
				}
			};

			// Update the reference with the concrete address
			let section = &mut self.sections[entry.section_index.0];
			let [hi, lo] = addr.to_be_bytes();
			section.data[entry.section_offset.0] |= hi;
			section.data[entry.section_offset.0 + 1] |= lo;
		}

		Ok(())
	}

	fn find_numeric_label_def_forward(
		&self,
		num: NumericSymbol,
		start_index: usize,
	) -> Result<(SectionIndex, SectionOffset)> {
		// Find the next definition of the numeric label after start_index
		for (_, entry) in self.numeric_label_list.iter().enumerate().skip(start_index) {
			if entry.entry_type == NumericLabelListEntryType::Definition && entry.symbol == num {
				return Ok((entry.section_index, entry.section_offset));
			}
		}
		Err(ChipError::UndefinedSymbol(format!(
			"Numeric label {} not found",
			num.0
		)))
	}

	fn find_numeric_label_def_backward(
		&self,
		num: NumericSymbol,
		start_index: usize,
	) -> Result<(SectionIndex, SectionOffset)> {
		// Find the next definition of the numeric label after start_index
		for (_, entry) in self
			.numeric_label_list
			.iter()
			.enumerate()
			.skip(start_index)
			.rev()
		{
			if entry.entry_type == NumericLabelListEntryType::Definition {
				if entry.symbol == num {
					return Ok((entry.section_index, entry.section_offset));
				}
			}
		}
		Err(ChipError::UndefinedSymbol(format!(
			"Numeric label {} not found",
			num.0
		)))
	}

	fn label_def(&mut self, token: &Token<'_>) -> Result<()> {
		// Get the label name
		let label = SymbolName(Rc::from(token.text()));

		// Add the symbol to the current section's symbol table
		let section: &mut Section = &mut self.sections[self.current_section.0];

		section
			.symbol_defs
			.insert(label.clone(), SectionOffset(section.data.len()));

		// Check if this symbol is global
		if label.is_global() {
			self.global_symbols
				.insert(label.clone(), self.current_section);
		}

		// Skip the colon
		self.expect_next_token(TokenType::Colon, "Expected colon after label")?;

		Ok(())
	}

	fn numeric_label_def(&mut self, token: &Token<'_>) -> Result<()> {
		// Get the label name
		let label = match token.token_type() {
			TokenType::Number(_, n) => NumericSymbol(n as u8),
			_ => unreachable!(),
		};

		// Check that the number is a valid numeric label
		if label.0 > 99 {
			return Err(ChipError::SyntaxError(
				String::from("Numeric label must be between 0 and 99"),
				token.pos().clone(),
			));
		}

		// Add the symbol def to the list of numeric symbols
		self.numeric_label_list.push(NumericLabelListEntry {
			entry_type: NumericLabelListEntryType::Definition,
			symbol: label,
			section_index: self.current_section,
			section_offset: SectionOffset(self.sections[self.current_section.0].data.len()),
		});

		// Skip the colon
		self.expect_next_token(TokenType::Colon, "Expected colon after label")?;

		Ok(())
	}

	fn instruction(&mut self, token: &Token<'_>, inst: Instruction) -> Result<()> {
		// Unique operands:
		// | Mode  | Format | Operand 1                 | Operand 2 |
		// |-------|--------|---------------------------|-----------|
		// | none  | 0000   |                           |           |
		// | short | 0nnn   | Literal or label, 12 bits |           |
		// | byte_reg | 0snn | Literal byte, 8 bits | Register s, 4 bits |
		//  - 0000: No operands
		// 	- 0nnn: Literal short or label (Can be an address). 12 bits.
		//  - 0snn: Register `s` and literal byte `nn`
		//  - 0st0: Register `s` and register `t`
		//

		match inst {
			Instruction::SYS => self.instruction_0nnn(0x0000, token)?,
			Instruction::CLR => self.instruction_0000(0x00E0)?,
			Instruction::RTS => self.instruction_0000(0x00EE)?,
			Instruction::JUMP => self.instruction_0nnn(0x1000, token)?,
			Instruction::CALL => self.instruction_0nnn(0x2000, token)?,
			Instruction::SKE => self.instruction_0snn(0x3000)?,
			Instruction::SKNE => self.instruction_0snn(0x4000)?,
			Instruction::SKRE => self.instruction_0st0(0x5000)?,
			Instruction::LOAD => self.instruction_0snn(0x6000)?,
			Instruction::ADD => self.instruction_0snn(0x7000)?,
			Instruction::MOVE => self.instruction_0st0(0x8000)?,
			Instruction::OR => self.instruction_0st0(0x8001)?,
			Instruction::AND => self.instruction_0st0(0x8002)?,
			Instruction::XOR => self.instruction_0st0(0x8003)?,
			Instruction::ADDR => self.instruction_0ts0(0x8004)?,
			Instruction::SUB => self.instruction_0ts0(0x8005)?,
			Instruction::SHR => self.instruction_0st0(0x8006)?,
			Instruction::SHL => self.instruction_0st0(0x800E)?,
			Instruction::SKRNE => self.instruction_0st0(0x9000)?,
			Instruction::LOADI => self.instruction_0nnn(0xA000, token)?,
			Instruction::JUMPI => self.instruction_0nnn(0xB000, token)?,
			Instruction::RAND => self.instruction_0snn(0xC000)?,
			Instruction::DRAW => self.instruction_0stn(0xD000)?,
			Instruction::SKPR => self.instruction_0s00(0xE09E)?,
			Instruction::SKUP => self.instruction_0s00(0xE0A1)?,
			Instruction::MOVED => self.instruction_0s00(0xF007)?,
			Instruction::KEYD => self.instruction_0s00(0xF00A)?,
			Instruction::LOADD => self.instruction_0s00(0xF015)?,
			Instruction::LOADS => self.instruction_0s00(0xF018)?,
			Instruction::ADDI => self.instruction_0s00(0xF01E)?,
			Instruction::LDSPR => self.instruction_0s00(0xF029)?,
			Instruction::BCD => self.instruction_0s00(0xF033)?,
			Instruction::STOR => self.instruction_0s00(0xF055)?,
			Instruction::READ => self.instruction_0s00(0xF065)?,
			Instruction::SCRD => self.instruction_000n(0x00C0)?,
			Instruction::SCRR => self.instruction_0000(0x00FB)?,
			Instruction::SCRL => self.instruction_0000(0x00FC)?,
			Instruction::EXIT => self.instruction_0000(0x00FD)?,
			Instruction::EXTD => self.instruction_0000(0x00FE)?,
			Instruction::EXTE => self.instruction_0000(0x00FF)?,
			Instruction::SRPL => self.instruction_0s00(0xF075)?,
			Instruction::LRPL => self.instruction_0s00(0xF085)?,
			Instruction::SCRU => self.instruction_000n(0x00D0)?,
			Instruction::SAVESUB => self.instruction_0st0(0x5002)?,
			Instruction::LOADSUB => self.instruction_0st0(0x5003)?,
			Instruction::AUDIO => self.instruction_0000(0xF002)?,
			Instruction::PLANE => self.instruction_0n00(0xF003)?,
			Instruction::PITCH => self.instruction_0s00(0xF03A)?,
		}

		self.expect_statement_end()?;

		Ok(())
	}

	/// Emit an instruction with no operands
	fn instruction_0000(&mut self, opcode: u16) -> Result<()> {
		// Emit the opcode
		self.emit_short(opcode);

		Ok(())
	}

	/// Emit an instruction with a short operand (literal or label)
	fn instruction_0nnn(&mut self, opcode: u16, token: &Token<'_>) -> Result<()> {
		assert_opcode_mask(opcode, 0xF000);

		// Since the operand can be a label, we need to check for both numeric
		// and symbolic labels.
		match self.token_itr.peek() {
			Some(token) if matches!(token.token_type(), TokenType::Number { .. }) => {
				let literal = self.expect_next_12bit()?;
				self.emit_short(opcode | literal);
			}
			Some(token) if token.token_type() == TokenType::Identifier => {
				let token = self.expect_next_token(TokenType::Identifier, "Expected label")?;
				let label = SymbolName(Rc::from(token.text()));
				let addr = self.sections[self.current_section.0].data.len();
				self.sections[self.current_section.0]
					.symbol_refs
					.insert(label, SectionOffset(addr));
				// Emit a placeholder for the label, the actual address will be
				// resolved later.
				self.emit_short(opcode);
			}
			Some(token) if matches!(token.token_type(), TokenType::LabelRef { .. }) => {
				let label = self.token_itr.next().unwrap();
				if let TokenType::LabelRef(num, dir) = label.token_type() {
					if dir == LabelRefDirection::Forward {
						self.numeric_label_list.push(NumericLabelListEntry {
							entry_type: NumericLabelListEntryType::ReferenceForward,
							symbol: NumericSymbol(num),
							section_index: self.current_section,
							section_offset: SectionOffset(
								self.sections[self.current_section.0].data.len(),
							),
						});
					} else if dir == LabelRefDirection::Backward {
						self.numeric_label_list.push(NumericLabelListEntry {
							entry_type: NumericLabelListEntryType::ReferenceBackward,
							symbol: NumericSymbol(num),
							section_index: self.current_section,
							section_offset: SectionOffset(
								self.sections[self.current_section.0].data.len(),
							),
						});
					} else {
						unreachable!();
					}
					// Emit a placeholder for the label, the actual address will be
					// resolved later.
					self.emit_short(opcode);
				} else {
					unreachable!();
				}
			}
			Some(_) => {
				return Err(ChipError::SyntaxError(
					String::from("Expected label or literal number"),
					token.pos().clone(),
				))
			}
			None => return Err(ChipError::UnexpectedEof),
		}

		Ok(())
	}

	/// Emit an instruction with a byte and register operand. f(nn) => s
	fn instruction_0snn(&mut self, opcode: u16) -> Result<()> {
		assert_opcode_mask(opcode, 0xF000);

		let literal = self.expect_next_byte()?;
		let register = self.expect_next_nibble()?;

		self.emit_short(opcode | ((register as u16) << 8) | (literal as u16));

		Ok(())
	}

	/// Emit an instruction with two register operands. f(s) => t
	fn instruction_0st0(&mut self, opcode: u16) -> Result<()> {
		assert_opcode_mask(opcode, 0xF00F);

		let s = self.expect_next_register()?;
		let t = self.expect_next_register()?;

		self.emit_short(opcode | ((s as u16) << 8) | ((t as u16) << 4));

		Ok(())
	}

	/// Emit an instruction with two register operands. f(s) => t
	fn instruction_0ts0(&mut self, opcode: u16) -> Result<()> {
		assert_opcode_mask(opcode, 0xF00F);

		let s = self.expect_next_register()?;
		let t = self.expect_next_register()?;

		self.emit_short(opcode | ((t as u16) << 8) | ((s as u16) << 4));

		Ok(())
	}

	/// Emit an instruction with a byte and two register operands. f(n, s) => t
	fn instruction_0stn(&mut self, opcode: u16) -> Result<()> {
		assert_opcode_mask(opcode, 0xF000);

		let n = self.expect_next_nibble()?;
		let s = self.expect_next_register()?;
		let t = self.expect_next_register()?;

		self.emit_short(opcode | ((s as u16) << 8) | ((t as u16) << 4) | n as u16);

		Ok(())
	}

	/// Emit an instruction with a register operand. f(s)
	fn instruction_0s00(&mut self, opcode: u16) -> Result<()> {
		assert_opcode_mask(opcode, 0xF0FF);

		let s = self.expect_next_register()?;
		self.emit_short(opcode | ((s as u16) << 8));

		Ok(())
	}

	/// Emit an instruction with a half byte operand. f(n)
	fn instruction_000n(&mut self, opcode: u16) -> Result<()> {
		assert_opcode_mask(opcode, 0xFFF0);

		let n = self.expect_next_nibble()?;

		self.emit_short(opcode | n as u16);

		Ok(())
	}

	/// Emit an instruction with a half byte operand. f(n)
	fn instruction_0n00(&mut self, opcode: u16) -> Result<()> {
		assert_opcode_mask(opcode, 0xF0FF);

		let n = self.expect_next_nibble()?;

		self.emit_short(opcode | ((n as u16) << 8));

		Ok(())
	}

	fn expect_next_token(
		&mut self,
		token_type: TokenType,
		error_message: &'static str,
	) -> Result<Token> {
		match self.token_itr.next() {
			Some(token) if token.token_type() == token_type => Ok(token),
			Some(token) => Err(ChipError::SyntaxError(
				String::from(error_message),
				token.pos().clone(),
			)),
			None => Err(ChipError::UnexpectedEof),
		}
	}

	fn expect_next_nibble(&mut self) -> Result<u8> {
		match self.token_itr.next() {
			Some(token) => match token.token_type() {
				TokenType::Number(_, num) => {
					if num > 0xF {
						Err(ChipError::SyntaxError(
							String::from("Value out of range, expected nibble"),
							token.pos().clone(),
						))
					} else {
						Ok(num as u8)
					}
				}
				_ => Err(ChipError::SyntaxError(
					String::from("Expected nibble"),
					token.pos().clone(),
				)),
			},
			None => Err(ChipError::UnexpectedEof),
		}
	}

	fn expect_next_byte(&mut self) -> Result<u8> {
		match self.token_itr.next() {
			Some(token) => match token.token_type() {
				TokenType::Number(_, num) => {
					if num > 0xFF {
						Err(ChipError::SyntaxError(
							"Value out of range, expected byte".to_string(),
							token.pos().clone(),
						))
					} else {
						Ok(num as u8)
					}
				}
				_ => Err(ChipError::SyntaxError(
					"Expected byte".to_string(),
					token.pos().clone(),
				)),
			},
			None => Err(ChipError::UnexpectedEof),
		}
	}

	fn expect_next_12bit(&mut self) -> Result<u16> {
		match self.token_itr.next() {
			Some(token) => match token.token_type() {
				TokenType::Number(_, num) => {
					if num > 0xFFF {
						Err(ChipError::SyntaxError(
							"Value out of range, expected 12 bit value".to_string(),
							token.pos().clone(),
						))
					} else {
						Ok(num as u16)
					}
				}
				_ => Err(ChipError::SyntaxError(
					"Expected 12 bit value".to_string(),
					token.pos().clone(),
				)),
			},
			None => Err(ChipError::UnexpectedEof),
		}
	}

	fn expect_next_register(&mut self) -> Result<u8> {
		match self.token_itr.next() {
			Some(token) => match token.token_type() {
				TokenType::Register(num) => {
					if num > 0xF {
						Err(ChipError::SyntaxError(
							"Value out of range, expected register 0x0 to 0xF".to_string(),
							token.pos().clone(),
						))
					} else {
						Ok(num as u8)
					}
				}
				_ => Err(ChipError::SyntaxError(
					"Expected register".to_string(),
					token.pos().clone(),
				)),
			},
			None => Err(ChipError::UnexpectedEof),
		}
	}

	fn expect_statement_end(&mut self) -> Result<()> {
		match self.token_itr.next() {
			Some(token) => match token.token_type() {
				// Valid end of statement tokens
				TokenType::Semicolon => Ok(()),
				TokenType::Newline => Ok(()),
				TokenType::EoF => Ok(()),
				// Skip comments
				TokenType::Comment => self.expect_statement_end(),
				// Unexpected token
				_ => Err(ChipError::SyntaxError(
					format!(
						"Expected end of statement, received {:?}",
						token.token_type()
					),
					token.pos().clone(),
				)),
			},
			// End of file is also a valid end of statement
			None => Ok(()),
		}
	}

	fn emit_short(&mut self, short: u16) {
		let [hi, lo] = short.to_be_bytes();
		let section: &mut Section = &mut self.sections[self.current_section.0];
		section.data.push(hi);
		section.data.push(lo);
	}
}

#[cfg(test)]
mod test {
	use super::*;

	#[test]
	fn test_simple() -> std::result::Result<(), String> {
		let source = r#"
		# This is a comment
main:	CLR
		SYS 0x204
		JUMP main
		"#;

		let mut asm = Assembler::new(Rc::from("example.txt"), source);
		asm.compile().map_err(|e| e.to_string())?;

		let idx = asm.get_section_index("text").unwrap();
		let section = asm.get_section(idx);

		assert_eq!(section.data[0..2], vec![0x00, 0xE0]);
		assert_eq!(section.data[2..4], vec![0x02, 0x04]);
		assert_eq!(section.data[4..6], vec![0x10, 0x00]);

		Ok(())
	}
}
