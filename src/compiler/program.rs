use std::{collections::HashMap, rc::Rc};

use crate::{
	error::{ChipError, Result},
	machine::{Memory, MEMORY_SIZE},
};

use super::types::{MemPtr, SymbolName};

pub struct Program {
	name: Rc<str>,
	data: Memory,

	symbols: HashMap<SymbolName, MemPtr>,
}

impl Program {
	pub fn new(name: Rc<str>) -> Self {
		Self {
			name,
			data: [0; MEMORY_SIZE],
			symbols: HashMap::new(),
		}
	}

	/// Adds a symbol to the program's symbol table
	pub fn add_symbol(&mut self, name: SymbolName, address: MemPtr) {
		self.symbols.insert(name, address);
	}

	pub fn has_symbol(&self, name: &SymbolName) -> bool {
		self.symbols.contains_key(name)
	}

	pub fn get_symbol(&self, name: &SymbolName) -> Option<&MemPtr> {
		self.symbols.get(name)
	}

	pub fn data(&self) -> &[u8] {
		&self.data
	}

	pub fn get_short(&self, addr: MemPtr) -> Result<u16> {
		if addr.0 as usize + 1 >= MEMORY_SIZE {
			return Err(ChipError::AddressOutOfBounds);
		}

		Ok(
			(u16::from(self.data[addr.0 as usize]) << 8)
				| u16::from(self.data[addr.0 as usize + 1]),
		)
	}

	pub fn set_short(&mut self, addr: MemPtr, value: u16) -> Result<()> {
		if addr.0 as usize + 1 >= MEMORY_SIZE {
			return Err(ChipError::AddressOutOfBounds);
		}

		self.data[addr.0 as usize] = (value >> 8) as u8;
		self.data[addr.0 as usize + 1] = value as u8;

		Ok(())
	}
}

impl std::fmt::Display for Program {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		write!(f, "Program: {}\n", self.name)?;

		let mut address = 0;

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

#[cfg(test)]
mod test {
	use super::*;

	#[test]
	fn test_section_display() {
		// let section = Section {
		// 	name: SectionName(Rc::from("test")),
		// 	start: Some(MemPtr(0x1000)),
		// 	data: vec![0x00, 0x01, 0x02, 0x03],
		// 	file_pos: Pos::new(Rc::from("example.txt"), 1, 1),
		// 	symbol_defs: HashMap::new(),
		// };

		// assert_eq!(
		// 	format!("{}", section),
		// 	"Section: test\n1000: 00 01 02 03 ....\n"
		// );
	}
}
