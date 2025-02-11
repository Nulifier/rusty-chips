use std::iter::Peekable;
use std::rc::Rc;
use std::str::CharIndices;

use crate::compiler::position::Pos;

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum NumberBase {
	Decimal,
	Binary,
	Octal,
	Hexadecimal,
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum LabelRefDirection {
	Forward,
	Backward,
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum TokenType {
	Comment,

	Comma,     // 0x54
	Dash,      // 0x55
	Dot,       // 0x56
	Colon,     // 0x72
	Semicolon, // 0x73

	Register(u8), // r2 or %2
	Identifier,
	Number(NumberBase, u32),
	String,
	LabelRef(u8, LabelRefDirection),

	Instruction(Instruction),

	Newline,
	EoF,
	Error,
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Instruction {
	ADD,
	ADDI,
	ADDR,
	AND,
	AUDIO,
	BCD,
	CALL,
	CLR,
	DRAW,
	EXIT,
	EXTD,
	EXTE,
	JUMP,
	JUMPI,
	KEYD,
	LDSPR,
	LOAD,
	LOADD,
	LOADI,
	LOADS,
	LOADSUB,
	LRPL,
	MOVE,
	MOVED,
	OR,
	PITCH,
	PLANE,
	RAND,
	READ,
	RTS,
	SAVESUB,
	SCRD,
	SCRL,
	SCRR,
	SCRU,
	SHL,
	SHR,
	SKE,
	SKNE,
	SKPR,
	SKRE,
	SKRNE,
	SKUP,
	SRPL,
	STOR,
	SUB,
	SYS,
	XOR,
}

#[derive(Debug)]
pub struct Token<'a> {
	token_type: TokenType,
	text: &'a str,
	pos: Pos,
}

impl<'a> Token<'a> {
	pub fn new(token_type: TokenType, text: &'a str, pos: Pos) -> Self {
		Self {
			token_type,
			text,
			pos,
		}
	}

	pub fn token_type(&self) -> TokenType {
		self.token_type
	}

	pub fn text(&self) -> &'a str {
		self.text
	}

	pub fn pos(&self) -> &Pos {
		&self.pos
	}
}

pub struct Scanner<'a> {
	source: &'a str,
	current: Peekable<CharIndices<'a>>,
	offset: usize,
	start: usize,
	filename: Rc<str>,
	line_num: usize,
	col_num: usize,
}

impl<'a> Scanner<'a> {
	/// This is the longest identifier that is a reserved word.
	const IDENTIFIER_MAX_LENGTH: usize = 7;

	pub fn new(filename: Rc<str>, source: &'a str) -> Self {
		Self {
			source,
			current: source.char_indices().peekable(),
			offset: 0,
			start: usize::MAX,
			filename,
			line_num: 1,
			col_num: 0, // Advance increments the column number so we start at 0
		}
	}

	pub fn next(&mut self) -> Token<'a> {
		self.skip_whitespace();

		let c = match self.advance() {
			Some(c) => c,
			None => return self.make_token(TokenType::EoF),
		};

		self.start_token();

		match c {
			',' => self.make_token(TokenType::Comma),
			'-' => self.make_token(TokenType::Dash),
			'.' => self.make_token(TokenType::Dot),
			':' => self.make_token(TokenType::Colon),
			';' => self.make_token(TokenType::Semicolon),
			'%' => self.register(),
			'a'..='z' | 'A'..='Z' | '_' => self.identifier(),
			'0'..='9' => self.number(c),
			'#' => self.comment(),
			'\n' => self.newline(),
			_ => self.error_token("Unexpected character"),
		}
	}

	fn make_token(&mut self, token_type: TokenType) -> Token<'a> {
		let text = if token_type == TokenType::EoF {
			"EOF"
		} else {
			// Determine what the next character is
			match self.current.peek() {
				// We have a next character
				Some((idx, _)) => &self.source[self.start..*idx],
				// We are at the end so slice the rest
				None => &self.source[self.start..],
			}
		};
		Token::new(token_type, text, self.get_pos())
	}

	fn error_token(&self, message: &'static str) -> Token<'static> {
		Token::new(TokenType::Error, message, self.get_pos())
	}

	fn skip_whitespace(&mut self) {
		loop {
			if let Some(c) = self.peek() {
				match c {
					' ' | '\r' | '\t' => {
						self.advance().unwrap(); // We already peeked so its safe to unwrap
					}
					_ => return, //
				}
			} else {
				return; // End of file
			}
		}
	}

	fn newline(&mut self) -> Token<'a> {
		self.line_num += 1;
		self.col_num = 1;
		self.make_token(TokenType::Newline)
	}

	fn comment(&mut self) -> Token<'a> {
		// Consume all characters until the end of line
		while let Some(_) = self.advance_if(|c| c != '\n') {}

		self.make_token(TokenType::Comment)
	}

	fn identifier(&mut self) -> Token<'a> {
		// Consume all identifier characters:
		// - Letters
		// - Numbers
		// - Underscore
		while let Some(_) = self.advance_if(|c| c.is_alphanumeric() || c == '_') {}

		let mut token = self.make_token(TokenType::Identifier);

		let text: Vec<char> = token
			.text()
			.chars()
			.take(Self::IDENTIFIER_MAX_LENGTH)
			.collect();

		token.token_type = match text[0].to_ascii_lowercase() {
			'a' if text.len() > 1 => match text[1].to_ascii_lowercase() {
				'd' if text.len() > 2 => match text[2].to_ascii_lowercase() {
					'd' if text.len() == 3 => TokenType::Instruction(Instruction::ADD),
					'd' if text.len() > 3 => match text[3].to_ascii_lowercase() {
						'i' if text.len() == 4 => TokenType::Instruction(Instruction::ADDI),
						'r' if text.len() == 4 => TokenType::Instruction(Instruction::ADDR),
						_ => TokenType::Identifier,
					},
					_ => TokenType::Identifier,
				},
				'n' => self.check_instruction(&text[2..], "d", Instruction::AND),
				'u' => self.check_instruction(&text[2..], "dio", Instruction::AUDIO),
				_ => TokenType::Identifier,
			},
			'b' => self.check_instruction(&text[1..], "cd", Instruction::BCD),
			'c' if text.len() > 1 => match text[1].to_ascii_lowercase() {
				'a' => self.check_instruction(&text[2..], "ll", Instruction::CALL),
				'l' => self.check_instruction(&text[2..], "r", Instruction::CLR),
				_ => TokenType::Identifier,
			},
			'd' => self.check_instruction(&text[1..], "raw", Instruction::DRAW),
			'e' if text.len() > 1 => match text[1].to_ascii_lowercase() {
				'x' if text.len() > 2 => match text[2].to_ascii_lowercase() {
					'i' => self.check_instruction(&text[3..], "t", Instruction::EXIT),
					't' if text.len() > 3 => match text[3].to_ascii_lowercase() {
						'd' if text.len() == 4 => TokenType::Instruction(Instruction::EXTD),
						'e' if text.len() == 4 => TokenType::Instruction(Instruction::EXTE),
						_ => TokenType::Identifier,
					},
					_ => TokenType::Identifier,
				},
				_ => TokenType::Identifier,
			},
			'j' if text.len() > 1 => match text[1].to_ascii_lowercase() {
				'u' if text.len() == 4 => {
					self.check_instruction(&text[2..], "mp", Instruction::JUMP)
				}
				'u' if text.len() == 5 => {
					self.check_instruction(&text[2..], "mpi", Instruction::JUMPI)
				}
				_ => TokenType::Identifier,
			},
			'k' => self.check_instruction(&text[1..], "eyd", Instruction::KEYD),
			'l' if text.len() > 1 => match text[1].to_ascii_lowercase() {
				'd' => self.check_instruction(&text[2..], "spr", Instruction::LDSPR),
				'o' if text.len() > 2 => match text[2].to_ascii_lowercase() {
					'a' if text.len() > 3 => match text[3].to_ascii_lowercase() {
						'd' if text.len() == 4 => TokenType::Instruction(Instruction::LOAD),
						'd' if text.len() > 4 => match text[4].to_ascii_lowercase() {
							'd' if text.len() == 5 => TokenType::Instruction(Instruction::LOADD),
							'i' if text.len() == 5 => TokenType::Instruction(Instruction::LOADI),
							's' if text.len() == 5 => TokenType::Instruction(Instruction::LOADS),
							's' => self.check_instruction(&text[5..], "ub", Instruction::LOADSUB),
							_ => TokenType::Identifier,
						},
						_ => TokenType::Identifier,
					},
					_ => TokenType::Identifier,
				},
				'r' => self.check_instruction(&text[2..], "pl", Instruction::LRPL),
				_ => TokenType::Identifier,
			},
			'm' if text.len() > 1 => match text[1].to_ascii_lowercase() {
				'o' if text.len() == 4 => {
					self.check_instruction(&text[2..], "ve", Instruction::MOVE)
				}
				'o' if text.len() == 5 => {
					self.check_instruction(&text[2..], "ved", Instruction::MOVED)
				}
				_ => TokenType::Identifier,
			},
			'o' => self.check_instruction(&text[1..], "r", Instruction::OR),
			'p' if text.len() > 1 => match text[1].to_ascii_lowercase() {
				'i' => self.check_instruction(&text[2..], "tch", Instruction::PITCH),
				'l' => self.check_instruction(&text[2..], "ane", Instruction::PLANE),
				_ => TokenType::Identifier,
			},
			'r' if text.len() > 1 => match text[1].to_ascii_lowercase() {
				'a' => self.check_instruction(&text[2..], "nd", Instruction::RAND),
				'e' => self.check_instruction(&text[2..], "ad", Instruction::READ),
				't' => self.check_instruction(&text[2..], "s", Instruction::RTS),
				_ => TokenType::Identifier,
			},
			's' if text.len() > 1 => match text[1].to_ascii_lowercase() {
				'a' => self.check_instruction(&text[2..], "vesub", Instruction::SAVESUB),
				'c' if text.len() > 2 => match text[2].to_ascii_lowercase() {
					'r' if text.len() > 3 => match text[3].to_ascii_lowercase() {
						'd' if text.len() == 4 => TokenType::Instruction(Instruction::SCRD),
						'l' if text.len() == 4 => TokenType::Instruction(Instruction::SCRL),
						'r' if text.len() == 4 => TokenType::Instruction(Instruction::SCRR),
						'u' if text.len() == 4 => TokenType::Instruction(Instruction::SCRU),
						_ => TokenType::Identifier,
					},
					_ => TokenType::Identifier,
				},
				'h' if text.len() > 2 => match text[2].to_ascii_lowercase() {
					'l' if text.len() == 3 => TokenType::Instruction(Instruction::SHL),
					'r' if text.len() == 3 => TokenType::Instruction(Instruction::SHR),
					_ => TokenType::Identifier,
				},
				'k' if text.len() > 2 => match text[2].to_ascii_lowercase() {
					'e' if text.len() == 3 => TokenType::Instruction(Instruction::SKE),
					'n' => self.check_instruction(&text[3..], "e", Instruction::SKNE),
					'p' => self.check_instruction(&text[3..], "r", Instruction::SKPR),
					'r' if text.len() == 4 => {
						self.check_instruction(&text[3..], "e", Instruction::SKRE)
					}
					'r' if text.len() == 5 => {
						self.check_instruction(&text[3..], "ne", Instruction::SKRNE)
					}
					'u' => self.check_instruction(&text[3..], "p", Instruction::SKUP),
					_ => TokenType::Identifier,
				},
				'r' => self.check_instruction(&text[2..], "pl", Instruction::SRPL),
				't' => self.check_instruction(&text[2..], "or", Instruction::STOR),
				'u' => self.check_instruction(&text[2..], "b", Instruction::SUB),
				'y' => self.check_instruction(&text[2..], "s", Instruction::SYS),
				_ => TokenType::Identifier,
			},
			'x' => self.check_instruction(&text[1..], "or", Instruction::XOR),
			_ => TokenType::Identifier,
		};

		token
	}

	fn check_instruction(&self, text: &[char], rest: &str, opcode: Instruction) -> TokenType {
		if text.len() != rest.len() {
			return TokenType::Identifier;
		}
		for (i, c) in rest.chars().enumerate() {
			if text[i].to_ascii_lowercase() != c {
				return TokenType::Identifier;
			}
		}
		TokenType::Instruction(opcode)
	}

	fn number(&mut self, first_char: char) -> Token<'a> {
		// We only handle integers

		// The can be of the following bases:
		// - Decimal (0-9)
		// - Binary (0-1), starting with 0b or 0B
		// - Octal (0-7), starting with 0o or 0O
		// - Hexadecimal (0-9, A-F), starting with 0x or 0X

		let mut base = NumberBase::Decimal;
		if first_char == '0' {
			if let Some(base_id) = self.advance_if(|c| {
				c == 'b' || c == 'B' || c == 'o' || c == 'O' || c == 'x' || c == 'X'
			}) {
				match base_id {
					'b' | 'B' => base = NumberBase::Binary,
					'o' | 'O' => base = NumberBase::Octal,
					'x' | 'X' => base = NumberBase::Hexadecimal,
					_ => unreachable!(),
				}
			};
		}

		// Consume all number characters
		match base {
			NumberBase::Decimal => while let Some(_) = self.advance_if(|c| c.is_numeric()) {},
			NumberBase::Binary => while let Some(_) = self.advance_if(|c| c == '0' || c == '1') {},
			NumberBase::Octal => {
				while let Some(_) = self.advance_if(|c| {
					c == '0'
						|| c == '1' || c == '2'
						|| c == '3' || c == '4'
						|| c == '5' || c == '6'
						|| c == '7'
				}) {}
			}
			NumberBase::Hexadecimal => {
				while let Some(_) = self.advance_if(|c| c.is_ascii_hexdigit()) {}
			}
		}

		while let Some(_) = self.advance_if(|c| c.is_numeric()) {}

		let mut token = self.make_token(TokenType::Number(base, 0));

		// Check that we don't have a stray "0x"
		if base != NumberBase::Decimal && token.text.len() <= 2 {
			return self.error_token("Invalid number");
		}

		// Parse the number
		let num = match base {
			NumberBase::Decimal => token.text.parse::<u32>(),
			NumberBase::Binary => u32::from_str_radix(&token.text[2..], 2),
			NumberBase::Octal => u32::from_str_radix(&token.text[2..], 8),
			NumberBase::Hexadecimal => u32::from_str_radix(&token.text[2..], 16),
		};

		if let Ok(num_val) = num {
			token.token_type = TokenType::Number(base, num_val);
			token
		} else {
			self.error_token("Invalid number")
		}
	}

	fn register(&mut self) -> Token<'a> {
		// The next character should be a hex value (0 to F)
		if let Some(c) = self.advance_if(|c| c.is_ascii_hexdigit()) {
			if let Some(reg) = c.to_digit(16) {
				self.make_token(TokenType::Register(reg as u8))
			} else {
				self.error_token("Registers must be a hex value after a %")
			}
		} else {
			self.error_token("Registers must be a hex value after a %")
		}
	}

	fn start_token(&mut self) {
		self.start = self.offset;
	}

	fn peek(&mut self) -> Option<char> {
		self.current.peek().map(|(_, c)| *c)
	}

	fn advance(&mut self) -> Option<char> {
		if let Some((idx, c)) = self.current.next() {
			self.col_num += 1;
			self.offset = idx;
			Some(c)
		} else {
			None
		}
	}

	fn advance_if<F: Fn(char) -> bool>(&mut self, f: F) -> Option<char> {
		if let Some(c) = self.peek() {
			if f(c) {
				return self.advance();
			}
		}
		None
	}

	fn get_pos(&self) -> Pos {
		Pos::new(self.filename.clone(), self.line_num, self.col_num)
	}
}

impl<'a> Iterator for Scanner<'a> {
	type Item = Token<'a>;

	fn next(&mut self) -> Option<Self::Item> {
		let token = self.next();
		if token.token_type() == TokenType::EoF {
			None
		} else {
			Some(token)
		}
	}
}

#[cfg(test)]
mod test {
	use super::*;

	#[test]
	fn test_comment() -> Result<(), String> {
		let filename = "example.txt";
		let source = "SYS # This is a comment\nADD";

		let mut scanner = Scanner::new(filename.into(), source);

		assert_eq!(
			scanner.next().token_type(),
			TokenType::Instruction(Instruction::SYS)
		);
		assert_eq!(scanner.next().token_type(), TokenType::Comment);
		assert_eq!(scanner.next().token_type(), TokenType::Newline);
		assert_eq!(
			scanner.next().token_type(),
			TokenType::Instruction(Instruction::ADD)
		);
		assert_eq!(scanner.next().token_type(), TokenType::EoF);

		Ok(())
	}

	#[test]
	fn test_punctuation() -> Result<(), String> {
		let filename = "example.txt";
		let source = ", - . : \n ;";

		let mut scanner = Scanner::new(filename.into(), source);

		assert_eq!(scanner.next().token_type(), TokenType::Comma);
		assert_eq!(scanner.next().token_type(), TokenType::Dash);
		assert_eq!(scanner.next().token_type(), TokenType::Dot);
		assert_eq!(scanner.next().token_type(), TokenType::Colon);
		assert_eq!(scanner.next().token_type(), TokenType::Newline);
		assert_eq!(scanner.next().token_type(), TokenType::Semicolon);
		assert_eq!(scanner.next().token_type(), TokenType::EoF);

		Ok(())
	}

	#[test]
	fn test_register() -> Result<(), String> {
		let filename = "example.txt";
		let source = "%0 %F";

		let mut scanner = Scanner::new(filename.into(), source);

		assert_eq!(scanner.next().token_type(), TokenType::Register(0));
		assert_eq!(scanner.next().token_type(), TokenType::Register(15));
		assert_eq!(scanner.next().token_type(), TokenType::EoF);

		Ok(())
	}

	#[test]
	fn test_identifier() -> Result<(), String> {
		let filename = "example.txt";
		let source = "test adding";

		let mut scanner = Scanner::new(filename.into(), source);

		assert_eq!(scanner.next().token_type(), TokenType::Identifier);
		assert_eq!(scanner.next().token_type(), TokenType::Identifier);
		assert_eq!(scanner.next().token_type(), TokenType::EoF);

		Ok(())
	}

	#[test]
	fn test_number() -> Result<(), String> {
		let filename = "example.txt";
		let source = "0 42 65535 05 0o77 0O77 0b1010 0B1010 0x1F 0X1F";

		let mut scanner = Scanner::new(filename.into(), source);

		assert_eq!(
			scanner.next().token_type(),
			TokenType::Number(NumberBase::Decimal, 0),
		);
		assert_eq!(
			scanner.next().token_type(),
			TokenType::Number(NumberBase::Decimal, 42),
		);
		assert_eq!(
			scanner.next().token_type(),
			TokenType::Number(NumberBase::Decimal, 65535),
		);
		assert_eq!(
			scanner.next().token_type(),
			TokenType::Number(NumberBase::Decimal, 5),
		);
		assert_eq!(
			scanner.next().token_type(),
			TokenType::Number(NumberBase::Octal, 0o77),
		);
		assert_eq!(
			scanner.next().token_type(),
			TokenType::Number(NumberBase::Octal, 0o77),
		);
		assert_eq!(
			scanner.next().token_type(),
			TokenType::Number(NumberBase::Binary, 0b1010),
		);
		assert_eq!(
			scanner.next().token_type(),
			TokenType::Number(NumberBase::Binary, 0b1010),
		);
		assert_eq!(
			scanner.next().token_type(),
			TokenType::Number(NumberBase::Hexadecimal, 0x1F),
		);
		assert_eq!(
			scanner.next().token_type(),
			TokenType::Number(NumberBase::Hexadecimal, 0x1F),
		);
		assert_eq!(scanner.next().token_type(), TokenType::EoF);

		Ok(())
	}

	#[test]
	fn test_opcodes() -> Result<(), String> {
		let filename = "example.txt";
		let source = "ADD ADDI ADDR AND AUDIO BCD CALL CLR DRAW EXIT EXTD EXTE JUMP JUMPI KEYD LDSPR LOAD LOADD LOADI LOADS LOADSUB LRPL MOVE MOVED OR PITCH PLANE RAND READ RTS SAVESUB SCRD SCRL SCRR SCRU SHL SHR SKE SKNE SKPR SKRE SKRNE SKUP SRPL STOR SUB SYS XOR";

		let mut scanner = Scanner::new(filename.into(), source);

		assert_eq!(
			scanner.next().token_type(),
			TokenType::Instruction(Instruction::ADD)
		);
		assert_eq!(
			scanner.next().token_type(),
			TokenType::Instruction(Instruction::ADDI)
		);
		assert_eq!(
			scanner.next().token_type(),
			TokenType::Instruction(Instruction::ADDR)
		);
		assert_eq!(
			scanner.next().token_type(),
			TokenType::Instruction(Instruction::AND)
		);
		assert_eq!(
			scanner.next().token_type(),
			TokenType::Instruction(Instruction::AUDIO)
		);
		assert_eq!(
			scanner.next().token_type(),
			TokenType::Instruction(Instruction::BCD)
		);
		assert_eq!(
			scanner.next().token_type(),
			TokenType::Instruction(Instruction::CALL)
		);
		assert_eq!(
			scanner.next().token_type(),
			TokenType::Instruction(Instruction::CLR)
		);
		assert_eq!(
			scanner.next().token_type(),
			TokenType::Instruction(Instruction::DRAW)
		);
		assert_eq!(
			scanner.next().token_type(),
			TokenType::Instruction(Instruction::EXIT)
		);
		assert_eq!(
			scanner.next().token_type(),
			TokenType::Instruction(Instruction::EXTD)
		);
		assert_eq!(
			scanner.next().token_type(),
			TokenType::Instruction(Instruction::EXTE)
		);
		assert_eq!(
			scanner.next().token_type(),
			TokenType::Instruction(Instruction::JUMP)
		);
		assert_eq!(
			scanner.next().token_type(),
			TokenType::Instruction(Instruction::JUMPI)
		);
		assert_eq!(
			scanner.next().token_type(),
			TokenType::Instruction(Instruction::KEYD)
		);
		assert_eq!(
			scanner.next().token_type(),
			TokenType::Instruction(Instruction::LDSPR)
		);
		assert_eq!(
			scanner.next().token_type(),
			TokenType::Instruction(Instruction::LOAD)
		);
		assert_eq!(
			scanner.next().token_type(),
			TokenType::Instruction(Instruction::LOADD)
		);
		assert_eq!(
			scanner.next().token_type(),
			TokenType::Instruction(Instruction::LOADI)
		);
		assert_eq!(
			scanner.next().token_type(),
			TokenType::Instruction(Instruction::LOADS)
		);
		assert_eq!(
			scanner.next().token_type(),
			TokenType::Instruction(Instruction::LOADSUB)
		);
		assert_eq!(
			scanner.next().token_type(),
			TokenType::Instruction(Instruction::LRPL)
		);
		assert_eq!(
			scanner.next().token_type(),
			TokenType::Instruction(Instruction::MOVE)
		);
		assert_eq!(
			scanner.next().token_type(),
			TokenType::Instruction(Instruction::MOVED)
		);
		assert_eq!(
			scanner.next().token_type(),
			TokenType::Instruction(Instruction::OR)
		);
		assert_eq!(
			scanner.next().token_type(),
			TokenType::Instruction(Instruction::PITCH)
		);
		assert_eq!(
			scanner.next().token_type(),
			TokenType::Instruction(Instruction::PLANE)
		);
		assert_eq!(
			scanner.next().token_type(),
			TokenType::Instruction(Instruction::RAND)
		);
		assert_eq!(
			scanner.next().token_type(),
			TokenType::Instruction(Instruction::READ)
		);
		assert_eq!(
			scanner.next().token_type(),
			TokenType::Instruction(Instruction::RTS)
		);
		assert_eq!(
			scanner.next().token_type(),
			TokenType::Instruction(Instruction::SAVESUB)
		);
		assert_eq!(
			scanner.next().token_type(),
			TokenType::Instruction(Instruction::SCRD)
		);
		assert_eq!(
			scanner.next().token_type(),
			TokenType::Instruction(Instruction::SCRL)
		);
		assert_eq!(
			scanner.next().token_type(),
			TokenType::Instruction(Instruction::SCRR)
		);
		assert_eq!(
			scanner.next().token_type(),
			TokenType::Instruction(Instruction::SCRU)
		);
		assert_eq!(
			scanner.next().token_type(),
			TokenType::Instruction(Instruction::SHL)
		);
		assert_eq!(
			scanner.next().token_type(),
			TokenType::Instruction(Instruction::SHR)
		);
		assert_eq!(
			scanner.next().token_type(),
			TokenType::Instruction(Instruction::SKE)
		);
		assert_eq!(
			scanner.next().token_type(),
			TokenType::Instruction(Instruction::SKNE)
		);
		assert_eq!(
			scanner.next().token_type(),
			TokenType::Instruction(Instruction::SKPR)
		);
		assert_eq!(
			scanner.next().token_type(),
			TokenType::Instruction(Instruction::SKRE)
		);
		assert_eq!(
			scanner.next().token_type(),
			TokenType::Instruction(Instruction::SKRNE)
		);
		assert_eq!(
			scanner.next().token_type(),
			TokenType::Instruction(Instruction::SKUP)
		);
		assert_eq!(
			scanner.next().token_type(),
			TokenType::Instruction(Instruction::SRPL)
		);
		assert_eq!(
			scanner.next().token_type(),
			TokenType::Instruction(Instruction::STOR)
		);
		assert_eq!(
			scanner.next().token_type(),
			TokenType::Instruction(Instruction::SUB)
		);
		assert_eq!(
			scanner.next().token_type(),
			TokenType::Instruction(Instruction::SYS)
		);
		assert_eq!(
			scanner.next().token_type(),
			TokenType::Instruction(Instruction::XOR)
		);
		assert_eq!(scanner.next().token_type(), TokenType::EoF);

		Ok(())
	}

	#[test]
	fn test_mixed() -> Result<(), String> {
		let filename = "example.txt";
		let source = r#"
		start: LOAD %0,42 # This is a basic test
		"#;

		let mut scanner = Scanner::new(filename.into(), source);

		assert_eq!(scanner.next().token_type(), TokenType::Newline);
		assert_eq!(scanner.next().token_type(), TokenType::Identifier);
		assert_eq!(scanner.next().token_type(), TokenType::Colon);
		assert_eq!(
			scanner.next().token_type(),
			TokenType::Instruction(Instruction::LOAD)
		);
		assert_eq!(scanner.next().token_type(), TokenType::Register(0));
		assert_eq!(scanner.next().token_type(), TokenType::Comma);
		assert_eq!(
			scanner.next().token_type(),
			TokenType::Number(NumberBase::Decimal, 42)
		);
		assert_eq!(scanner.next().token_type(), TokenType::Comment);
		assert_eq!(scanner.next().token_type(), TokenType::Newline);
		assert_eq!(scanner.next().token_type(), TokenType::EoF);

		Ok(())
	}
}
