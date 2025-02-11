use std::collections::HashMap;

use crate::error::{ChipError, Result};

use super::{
	position::Pos,
	types::{MemOffset, MemPtr, SectionName, SymbolName},
};

pub struct NumericLabel {
	id: u8,
	label_type: NumericLabelType,
	offset: MemOffset,
}

#[derive(PartialEq)]
pub enum NumericLabelType {
	Definition,
	ForwardReference,
	BackwardReference,
}

pub struct Section {
	name: SectionName,
	start: Option<MemPtr>,
	data: Vec<u8>,
	file_pos: Pos,
	symbols: HashMap<SymbolName, MemPtr>,
	numeric_labels: Vec<NumericLabel>,
}

impl Section {
	pub fn new(name: SectionName, file_pos: Pos) -> Self {
		Self {
			name,
			start: None,
			data: Vec::new(),
			file_pos,
			symbols: HashMap::new(),
			numeric_labels: Vec::new(),
		}
	}

	pub fn new_with_start(name: SectionName, start: MemPtr, file_pos: Pos) -> Self {
		Self {
			name,
			start: Some(start),
			data: Vec::new(),
			file_pos,
			symbols: HashMap::new(),
			numeric_labels: Vec::new(),
		}
	}

	pub fn name(&self) -> &SectionName {
		&self.name
	}

	pub fn data(&self) -> &Vec<u8> {
		&self.data
	}

	pub fn data_mut(&mut self) -> &mut Vec<u8> {
		&mut self.data
	}

	pub fn file_pos(&self) -> &Pos {
		&self.file_pos
	}

	pub fn add_symbol(&mut self, name: SymbolName, offset: MemPtr) {
		self.symbols.insert(name, offset);
	}

	pub fn symbol_iter(&self) -> impl Iterator<Item = (&SymbolName, &MemPtr)> {
		self.symbols.iter()
	}

	pub fn numeric_label_def(&mut self, id: u8, offset: MemOffset) {
		self.numeric_labels.push(NumericLabel {
			id,
			label_type: NumericLabelType::Definition,
			offset,
		});
	}

	pub fn forward_numeric_label_ref(&mut self, id: u8, offset: MemOffset) {
		self.numeric_labels.push(NumericLabel {
			id,
			label_type: NumericLabelType::ForwardReference,
			offset,
		});
	}

	pub fn backward_numeric_label_ref(&mut self, id: u8, offset: MemOffset) {
		self.numeric_labels.push(NumericLabel {
			id,
			label_type: NumericLabelType::BackwardReference,
			offset,
		});
	}

	pub fn numeric_label_def_iter(&self) -> impl Iterator<Item = &NumericLabel> {
		self.numeric_labels
			.iter()
			.filter(|label| label.label_type == NumericLabelType::Definition)
	}

	pub fn numeric_label_ref_iter(&self) -> impl Iterator<Item = &NumericLabel> {
		self.numeric_labels
			.iter()
			.filter(|label| label.label_type != NumericLabelType::Definition)
	}

	pub fn resolve_labels(&mut self, global_symbols: &HashMap<SymbolName, MemPtr>) -> Result<()> {
		// @TODO Verify
		for (symbol_name, symbol_offset) in &self.symbols {
			if let Some(global_offset) = global_symbols.get(symbol_name) {
				let address = global_offset.0 + symbol_offset.0;
				let address = MemPtr(address);

				self.data[symbol_offset.0 as usize] = (address.0 >> 8) as u8;
				self.data[symbol_offset.0 as usize + 1] = (address.0 & 0xff) as u8;
			} else {
				return Err(ChipError::UndefinedSymbol(symbol_name.0.to_string()));
			}
		}

		Ok(())
	}
}

impl std::fmt::Display for Section {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		write!(f, "Section: {}\n", self.name.0)?;

		let mut address = self.start.unwrap_or(MemPtr(0)).0;

		// For each set of 16 bytes
		for chunk in self.data.chunks(16) {
			// Write the address
			write!(f, "{:04x}: ", address)?;

			// Write the hex values
			for byte in chunk {
				write!(f, "{:02x} ", byte)?;
			}

			// Write the ASCII values
			for byte in chunk {
				write!(
					f,
					"{}",
					// If the byte is printable, write it as a character, otherwise write a dot
					if *byte >= 32 && *byte <= 126 {
						*byte as char
					} else {
						'.'
					}
				)?;
			}

			// Write a newline
			write!(f, "\n")?;

			address += 16;
		}

		Ok(())
	}
}

pub struct Program {
	sections: Vec<Section>,
}

impl Program {
	pub fn new() -> Self {
		Self {
			sections: Vec::new(),
		}
	}

	pub fn add_section(&mut self, section: Section) -> Result<()> {
		// Verify that there isn't a section with this name already
		if self.sections.iter().any(|s| s.name == section.name) {
			return Err(ChipError::DuplicationSection(section.name().0.to_string()));
		}

		self.sections.push(section);

		Ok(())
	}

	pub fn get_section_index(&self, name: &SectionName) -> Option<usize> {
		self.sections.iter().position(|s| s.name == *name)
	}

	pub fn get_section(&self, index: usize) -> Option<&Section> {
		self.sections.get(index)
	}

	pub fn get_section_mut(&mut self, index: usize) -> Option<&mut Section> {
		self.sections.get_mut(index)
	}

	pub fn section_iter(&self) -> impl Iterator<Item = &Section> {
		self.sections.iter()
	}
}

impl std::fmt::Display for Program {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		for section in &self.sections {
			write!(f, "{}", section)?;
		}

		Ok(())
	}
}

#[cfg(test)]
mod test {
	use super::*;
	use std::rc::Rc;

	#[test]
	fn test_section_display() {
		let section = Section {
			name: SectionName(Rc::from("test")),
			start: Some(MemPtr(0x1000)),
			data: vec![0x00, 0x01, 0x02, 0x03],
			file_pos: Pos::new(Rc::from("example.txt"), 1, 1),
			symbols: HashMap::new(),
		};

		assert_eq!(
			format!("{}", section),
			"Section: test\n1000: 00 01 02 03 ....\n"
		);
	}
}
