use crate::{
	compiler::{
		assembler::Assembler,
		types::{SectionName, SymbolName},
	},
	error::{ChipError, Result},
	machine::{Memory, MEMORY_SIZE},
};
use std::{cell::RefCell, collections::HashMap, rc::Rc};

use super::program::Program;
pub struct Linker<'a> {
	mem: Memory,
	program: &'a Program,
	units: Vec<Rc<RefCell<Assembler<'a, 'a>>>>,
	unit_section_names: Vec<Vec<SectionName>>,
	unit_section_offsets: Vec<Vec<usize>>,
	unit_section_sizes: Vec<Vec<usize>>,
	symbols: HashMap<SymbolName, usize>,
}

impl<'a> Linker<'a> {
	pub fn new(program: &'a Program) -> Self {
		Self {
			mem: [0; MEMORY_SIZE],
			program,
			units: Vec::new(),
			unit_section_names: Vec::new(),
			unit_section_offsets: Vec::new(),
			unit_section_sizes: Vec::new(),
			symbols: HashMap::new(),
		}
	}

	/// Add an assembler unit to the linker
	pub fn add_unit(&mut self, unit: Rc<RefCell<Assembler<'a, 'a>>>) {
		self.units.push(unit);
	}

	pub fn link(&mut self) -> Result<()> {
		let mut offset = 0;
		// Locate all the sections in the memory, building the global symbol table
		for (unit_index, unit) in self.units.iter().enumerate() {
			// Initialize the unit section vectors
			self.unit_section_names.push(Vec::new());
			self.unit_section_offsets.push(Vec::new());
			self.unit_section_sizes.push(Vec::new());

			// For each section in the unit
			for section in unit.borrow().sections() {
				let section_size = section.data().len();

				self.unit_section_names[unit_index].push(section.name().clone());
				self.unit_section_offsets[unit_index].push(offset);
				self.unit_section_sizes[unit_index].push(section_size);

				// Update the offset
				offset += section_size;

				// Add the symbols to the global symbol table
				for (symbol_name, symbol_offset) in section.symbols() {
					if symbol_name.is_global() {
						let symbol_address = offset + symbol_offset.0;
						self.symbols.insert(symbol_name.clone(), symbol_address);
					}
				}
			}
		}

		// Check that the program fits in memory
		if offset > MEMORY_SIZE {
			return Err(ChipError::ProgramTooLarge);
		}

		// Write the sections to the memory, allowing the units to locate
		for (unit_index, unit) in self.units.iter().enumerate() {
			// Location
			unit.borrow_mut()
				.locate_sections(&self.unit_section_offsets[unit_index], &self.symbols)?;

			// Write the sections to the memory
			for (section_index, section) in unit.borrow().sections().iter().enumerate() {
				let section_offset = self.unit_section_offsets[unit_index][section_index];
				let section_data = section.data();

				self.mem[section_offset..section_offset + section_data.len()]
					.copy_from_slice(section_data);
			}
		}

		Ok(())
	}
}

#[cfg(test)]
mod test {
	use super::*;

	#[test]
	fn test_linker() -> std::result::Result<(), Box<dyn std::error::Error>> {
		let source_a = r#"
main: 	CLR
		SYS 0x204
		JUMP main
		"#;

		let mut linker = Linker::new();
		let unit = Rc::new(RefCell::new(Assembler::new(
			Rc::from("example.txt"),
			source_a,
		)));
		unit.borrow_mut().compile()?;

		linker.add_unit(unit);
		linker.link()?;

		// // Print memory, 8 bytes to a line
		// for (i, byte) in linker.mem.iter().enumerate() {
		// 	if i % 8 == 0 {
		// 		println!();
		// 	}
		// 	print!("{:02X} ", byte);
		// }

		assert_eq!(linker.mem[0], 0x00);
		assert_eq!(linker.mem[1], 0xE0);
		assert_eq!(linker.mem[2], 0x02);
		assert_eq!(linker.mem[3], 0x04);
		assert_eq!(linker.mem[4], 0x10);
		assert_eq!(linker.mem[5], 0x00);

		Ok(())
	}
}
