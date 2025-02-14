use crate::compiler::position::Pos;
use std::fmt;

#[derive(Debug)]
pub enum ChipError {
	IoError(std::io::Error),
	AddressOutOfBounds,
	UnexpectedInput,
	UnexpectedEof,
	SyntaxError(String, Pos),
	UndefinedSymbol(String),
	DuplicateSymbol(String, Pos),
	ProgramTooLarge,
}

impl std::error::Error for ChipError {}

impl fmt::Display for ChipError {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		match self {
			ChipError::IoError(e) => e.fmt(f),
			ChipError::AddressOutOfBounds => write!(f, "Address out of bounds"),
			ChipError::UnexpectedInput => write!(f, "Unexpected input"),
			ChipError::UnexpectedEof => write!(f, "Unexpected end of file"),
			ChipError::SyntaxError(c, pos) => write!(f, "Syntax error: {}, at {}", c, pos),
			ChipError::UndefinedSymbol(s) => write!(f, "Undefined symbol: {}", s),
			ChipError::DuplicateSymbol(s, pos) => write!(f, "Duplicate symbol: {} at {}", s, pos),
			ChipError::ProgramTooLarge => write!(f, "Program too large to fit in memory"),
		}
	}
}

impl From<std::io::Error> for ChipError {
	fn from(e: std::io::Error) -> Self {
		ChipError::IoError(e)
	}
}

pub type Result<T> = std::result::Result<T, ChipError>;
