use super::program::Program;
use super::scanner::{Instruction, LabelRefDirection, Scanner, Token, TokenType};
use super::types::MemPtr;
use crate::compiler::types::SymbolName;
use crate::error::{ChipError, Result};
use std::collections::VecDeque;
use std::iter::Peekable;
use std::{collections::HashMap, rc::Rc};

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

pub struct Assembler<'a, 'b> {
	/// Scanner of the source file
	//scanner: Scanner<'a>,
	token_itr: Peekable<Scanner<'a>>,

	/// Program to compile the source into
	program: &'b mut Program,

	/// Current position in the program memory
	cursor: MemPtr,

	/// Undefined symbols that need to be resolved
	undefined_symbols: HashMap<SymbolName, MemPtr>,

	/// The last numeric labels that were defined for backwards labels
	backward_numeric_labels: HashMap<u8, MemPtr>,

	/// Forward numeric labels that need to be resolved
	forward_numeric_labels: HashMap<u8, VecDeque<MemPtr>>,
}

impl<'a, 'b> Assembler<'a, 'b> {
	pub fn new(filename: Rc<str>, source: &'a str, program: &'b mut Program) -> Self {
		let scanner = Scanner::new(filename, source);
		Self {
			token_itr: scanner.into_iter().peekable(),
			program,
			cursor: MemPtr(0),
			undefined_symbols: HashMap::new(),
			backward_numeric_labels: HashMap::new(),
			forward_numeric_labels: HashMap::new(),
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
				TokenType::Dot => self.directive()?,
				TokenType::EoF => break,

				TokenType::Instruction(inst) => self.instruction(&token, inst)?,

				_ => unreachable!("Unexpected token {:?}", token.token_type()),
			}
		}

		// Error out on any unresolved forward references
		if !self.forward_numeric_labels.is_empty() {
			return Err(ChipError::UndefinedSymbol(
				"Undefined numeric label references found".to_string(),
			));
		}

		// Resolve symbolic references
		for (symbol, addr) in self.undefined_symbols.iter() {
			if let Some(symbol_addr) = self.program.get_symbol(symbol) {
				self.program
					.set_short(*addr, self.program.get_short(*addr)? | symbol_addr.0)?;
			} else {
				return Err(ChipError::UndefinedSymbol(format!(
					"Symbol {} not found",
					symbol.0
				)));
			}
		}

		Ok(())
	}

	fn label_def(&mut self, token: &Token<'_>) -> Result<()> {
		// Get the label name
		let label = SymbolName(Rc::from(token.text()));

		// Check that this isn't a duplicate symbol
		if self.program.has_symbol(&label) {
			return Err(ChipError::DuplicateSymbol(
				String::from("Duplicate symbol"),
				token.pos().clone(),
			));
		}

		// Add the symbol to the program's symbol table
		self.program.add_symbol(label.clone(), self.cursor.clone());

		// Skip the colon
		self.expect_next_token(TokenType::Colon, "Expected colon after label")?;

		Ok(())
	}

	fn numeric_label_def(&mut self, token: &Token<'_>) -> Result<()> {
		let addr = self.cursor;

		// Get the label number
		let label = match token.token_type() {
			TokenType::Number(_, n) => n as u8,
			_ => unreachable!(),
		};

		// Check that the number is a valid numeric label
		if label > 99 {
			return Err(ChipError::SyntaxError(
				String::from("Numeric label must be between 0 and 99"),
				token.pos().clone(),
			));
		}

		// Record the location of this label for future backward references
		self.backward_numeric_labels.insert(label, self.cursor);

		// Check for any forward references to this label
		if let Some(forward_refs) = self.forward_numeric_labels.remove(&label) {
			// Update each of the forward references with the current address
			for forward_ref in forward_refs {
				// Since the bits in the placeholder opcode are zero, or the address with them
				self.program
					.set_short(forward_ref, self.program.get_short(addr)? | addr.0)?;
			}
		}

		// Skip the colon
		self.expect_next_token(TokenType::Colon, "Expected colon after label")?;

		Ok(())
	}

	fn directive(&mut self) -> Result<()> {
		let identifier = self.expect_next_token(
			TokenType::Identifier,
			"Directives must be an identifier following a dot",
		)?;

		match identifier.text() {
			"start" => {
				self.cursor = MemPtr(0x200);
			}
			_ => {
				return Err(ChipError::SyntaxError(
					format!("Unknown directive: {}", identifier.text()),
					identifier.pos().clone(),
				))
			}
		}

		self.expect_statement_end()?;

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
		self.emit_short(opcode)
	}

	/// Emit an instruction with a short operand (literal or label)
	fn instruction_0nnn(&mut self, opcode: u16, token: &Token<'_>) -> Result<()> {
		assert_opcode_mask(opcode, 0xF000);

		// Since the operand can be a label, we need to check for both numeric
		// and symbolic labels.
		match self.token_itr.peek() {
			Some(token) if matches!(token.token_type(), TokenType::Number { .. }) => {
				let literal = self.expect_next_12bit()?;
				self.emit_short(opcode | literal)?;
			}
			Some(token) if token.token_type() == TokenType::Identifier => {
				let token = self.expect_next_token(TokenType::Identifier, "Expected label")?;
				let label = SymbolName(Rc::from(token.text()));

				// Remember to come back for this one later
				self.undefined_symbols.insert(label, self.cursor);

				// Emit a placeholder for the label, the actual address will be
				// resolved later.
				self.emit_short(opcode)?;
			}
			Some(token) if matches!(token.token_type(), TokenType::LabelRef { .. }) => {
				let label = self.token_itr.next().unwrap();
				if let TokenType::LabelRef(num, dir) = label.token_type() {
					let addr = if dir == LabelRefDirection::Forward {
						// We're looking for the next numerical symbol in the program
						// Since we can't see the future, we'll record it for later
						self.forward_numeric_labels
							.entry(num)
							.or_insert_with(|| VecDeque::new())
							.push_back(self.cursor);
						MemPtr(0)
					} else if dir == LabelRefDirection::Backward {
						// We're looking for the last time we defined this label
						*self.backward_numeric_labels.get(&num).ok_or(
							ChipError::UndefinedSymbol(format!("Numeric label {} not found", num)),
						)?
					} else {
						unreachable!();
					};
					// We may have the address, emit the instruction
					self.emit_short(opcode | addr.0)?;
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

		self.emit_short(opcode | ((register as u16) << 8) | (literal as u16))
	}

	/// Emit an instruction with two register operands. f(s) => t
	fn instruction_0st0(&mut self, opcode: u16) -> Result<()> {
		assert_opcode_mask(opcode, 0xF00F);

		let s = self.expect_next_register()?;
		let t = self.expect_next_register()?;

		self.emit_short(opcode | ((s as u16) << 8) | ((t as u16) << 4))
	}

	/// Emit an instruction with two register operands. f(s) => t
	fn instruction_0ts0(&mut self, opcode: u16) -> Result<()> {
		assert_opcode_mask(opcode, 0xF00F);

		let s = self.expect_next_register()?;
		let t = self.expect_next_register()?;

		self.emit_short(opcode | ((t as u16) << 8) | ((s as u16) << 4))
	}

	/// Emit an instruction with a byte and two register operands. f(n, s) => t
	fn instruction_0stn(&mut self, opcode: u16) -> Result<()> {
		assert_opcode_mask(opcode, 0xF000);

		let n = self.expect_next_nibble()?;
		let s = self.expect_next_register()?;
		let t = self.expect_next_register()?;

		self.emit_short(opcode | ((s as u16) << 8) | ((t as u16) << 4) | n as u16)
	}

	/// Emit an instruction with a register operand. f(s)
	fn instruction_0s00(&mut self, opcode: u16) -> Result<()> {
		assert_opcode_mask(opcode, 0xF0FF);

		let s = self.expect_next_register()?;
		self.emit_short(opcode | ((s as u16) << 8))
	}

	/// Emit an instruction with a half byte operand. f(n)
	fn instruction_000n(&mut self, opcode: u16) -> Result<()> {
		assert_opcode_mask(opcode, 0xFFF0);

		let n = self.expect_next_nibble()?;

		self.emit_short(opcode | n as u16)
	}

	/// Emit an instruction with a half byte operand. f(n)
	fn instruction_0n00(&mut self, opcode: u16) -> Result<()> {
		assert_opcode_mask(opcode, 0xF0FF);

		let n = self.expect_next_nibble()?;

		self.emit_short(opcode | ((n as u16) << 8))
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

	fn emit_short(&mut self, short: u16) -> Result<()> {
		self.program.set_short(self.cursor, short)?;
		self.cursor.0 += 2;
		Ok(())
	}
}

#[cfg(test)]
mod test {
	use super::*;

	#[test]
	fn test_assembler() -> std::result::Result<(), String> {
		let source = r#"
		# This is a comment
		AUDIO
main:	CLR
		SYS 0x204
1:		JUMP main
		JUMP 2f
		JUMP 1b
2:		EXIT
		"#;

		let mut prog = Program::new(Rc::from("test"));
		let mut asm = Assembler::new(Rc::from("example.txt"), source, &mut prog);
		asm.compile().map_err(|e| e.to_string())?;

		assert_eq!(prog.data()[0..2], vec![0xF0, 0x02]); // AUDIO
		assert_eq!(prog.data()[2..4], vec![0x00, 0xE0]); // CLR
		assert_eq!(prog.data()[4..6], vec![0x02, 0x04]); // SYS 0x204
		assert_eq!(prog.data()[6..8], vec![0x10, 0x02]); // JUMP main
		assert_eq!(prog.data()[10..12], vec![0x10, 0x06]); // JUMP 2f
		assert_eq!(prog.data()[10..12], vec![0x10, 0x06]); // JUMP 1b
		assert_eq!(prog.data()[12..14], vec![0x00, 0xFD]); // EXIT

		println!("{}", prog);

		Ok(())
	}
}
