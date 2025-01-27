use crate::assembler::position::Pos;
use std::fmt;

#[derive(Debug)]
pub enum ChipError {
	IoError(std::io::Error),
	UnexpectedInput,
	UnexpectedEof,
	SyntaxError(String, Pos),
	UndefinedSymbol(String),
}

impl std::error::Error for ChipError {}

impl fmt::Display for ChipError {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self {
			ChipError::IoError(e) => e.fmt(f),
			ChipError::UnexpectedInput => write!(f, "Unexpected input"),
			ChipError::UnexpectedEof => write!(f, "Unexpected end of file"),
			ChipError::SyntaxError(c, pos) => write!(f, "Syntax error: {}, at {}", c, pos),
			ChipError::UndefinedSymbol(s) => write!(f, "Undefined symbol: {}", s),
		}
	}
}

impl From<std::io::Error> for ChipError {
	fn from(e: std::io::Error) -> Self {
		ChipError::IoError(e)
	}
}

pub type Result<T> = std::result::Result<T, ChipError>;
