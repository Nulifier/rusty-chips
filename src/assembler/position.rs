use std::fmt;
use std::rc::Rc;

#[derive(Debug, Clone, PartialEq)]
pub struct Pos {
	pub filename: Rc<str>,
	pub line: usize,
	pub column: usize,
}

impl Pos {
	pub fn new(filename: Rc<str>, line: usize, column: usize) -> Pos {
		Pos {
			filename,
			line,
			column,
		}
	}
}

impl fmt::Display for Pos {
	fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
		write!(
			f,
			"{} [Ln {}, Col {}]",
			self.filename, self.line, self.column
		)
	}
}
