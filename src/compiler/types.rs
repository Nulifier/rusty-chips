use std::rc::Rc;

// This is a simple wrapper around a u16 to represent a memory pointer
// This is different from MemOffset as this is an absolute address.
#[derive(Debug, Clone, Copy)]
pub struct MemPtr(pub u16);

impl std::fmt::Display for MemPtr {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "{:04x}", self.0)
	}
}

// This is a simple wrapper around a u16 to represent a memory offset.
// This is different from MemPtr as this is an offset from the start of a section.
#[derive(Debug, Clone, Copy)]
pub struct MemOffset(pub u16);

impl std::fmt::Display for MemOffset {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "{:04x}", self.0)
	}
}

// This is a simple wrapper around a string to represent a symbol name
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct SymbolName(pub Rc<str>);

impl SymbolName {
	// Check if the symbol is local
	pub fn is_local(&self) -> bool {
		self.0.starts_with('_')
	}

	// Check if the symbol is global
	pub fn is_global(&self) -> bool {
		!self.0.starts_with('_')
	}
}
