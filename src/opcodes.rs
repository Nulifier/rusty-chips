#[derive(Debug, Copy, Clone, PartialEq)]
pub struct RegRef(u8);

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Opcode {
	///////////////////////////////////
	// Chip 8 Opcodes                //
	///////////////////////////////////
	//
	/// System call (Ignored)
	SYS { nnn: u16 },
	/// Clear the screen
	CLR,
	/// Return from subroutine
	RTS,
	/// Jump to address `nnn`
	JUMP { nnn: u16 },
	/// Call routine at address `nnn`
	CALL { nnn: u16 },
	/// Skip next instruction if register `s` equals `nn`
	SKE { s: RegRef, nn: u8 },
	/// Do not skip next instruction if register `s` equals `nn`
	SKNE { s: RegRef, nn: u8 },
	/// Skip next instruction if register `s` equals register `t`
	SKRE { s: RegRef, t: RegRef },
	/// Load register `s` with value `nn`
	LOAD { s: RegRef, nn: u8 },
	/// Add value `nn` to register `s`
	ADD { s: RegRef, nn: u8 },
	/// Move value from register `s` to register `t`
	MOVE { s: RegRef, t: RegRef },
	/// Perform logical OR on register `s` and `t` and store in `t`
	OR { s: RegRef, t: RegRef },
	/// Perform logical AND on register `s` and `t` and store in `t`
	AND { s: RegRef, t: RegRef },
	/// Perform logical XOR on regiser `s` and `t` and store in `t`
	XOR { s: RegRef, t: RegRef },
	/// Add `s` to `t` and store in `s`. Register F set on carry
	ADDR { s: RegRef, t: RegRef },
	/// Subtract `s` from `t` and store in `s`. Register `F` set on !borrow
	SUB { s: RegRef, t: RegRef },
	/// Shift bits in `s` 1 bit right, store in `t`. Bit 0 shifts to register `F`
	SHR { s: RegRef, t: RegRef },
	/// Shift bits in `s` 1 bit left, store in `t`. Bit 7 shifts to register `F`
	SHL { s: RegRef, t: RegRef },
	/// Skip next instruction if register `s` not equal register `t`
	SKRNE { s: RegRef, t: RegRef },
	/// Load index with value `nnn`
	LOADI { nnn: u16 },
	/// Jump to address `nnn` + index
	JUMPI { nnn: u16 },
	/// Generate random number between 0 and `nn` and store in `t`
	RAND { t: RegRef, nn: u8 },
	/// Draw `n` byte sprite at x location register `s`, y location reg `t`
	DRAW { s: RegRef, t: RegRef, n: u8 },
	/// Skip next instruction if the key in reg `s` is pressed
	SKPR { s: RegRef },
	/// Skip next instruction if hte key is reg `s` is not pressed
	SKUP { s: RegRef },
	/// Move delay timer value into register `t`
	MOVED { t: RegRef },
	/// Wait for keypress and store in register `t`
	KEYD { t: RegRef },
	/// Load delay timer with value in register `s`
	LOADD { s: RegRef },
	/// Load sound timer with value in register `s`
	LOADS { s: RegRef },
	/// Add value in register `s` to index
	ADDI { s: RegRef },
	/// Load index with sprite from register `s`
	LDSPR { s: RegRef },
	/// Store the binary coded decimal value of register `s` at index
	BCD { s: RegRef },
	/// Store the values of register `s` registers at index
	STOR { s: RegRef },
	/// Read back the stored values at index into registers
	READ { s: RegRef },

	///////////////////////////////////
	// Super Chip 8 Opcodes          //
	///////////////////////////////////
	//
	/// Scroll down `n` lines
	SCRD { n: u8 },
	/// Scroll right 4 pixels
	SCRR,
	/// Scroll left 4 pixels
	SCRL,
	/// Exit interpreter
	EXIT,
	/// Disable extended mode
	EXTD,
	/// Enable extended mode
	EXTE,
	/// Store subset of registers in RPL store
	SRPL { s: RegRef },
	/// Read back subset of registers from RPL store
	LRPL { s: RegRef },

	///////////////////////////////////
	// XO Chip Opcodes               //
	///////////////////////////////////
	//
	/// Scrolls the current bitplane up `n` pixels
	SCRU { n: u8 },
	/// Saves subset of registers from `s` to `t` in memory
	SAVESUB { s: RegRef, t: RegRef },
	/// Loads subset of registers from `s` to `t` from memory
	LOADSUB { s: RegRef, t: RegRef },
	/// Load 16-byte audio pattern buffer from index
	AUDIO,
	/// Sets the drawing bitplane to `n`
	PLANE { n: u8 },
	/// Sets the internal pitch to the value in register `s`
	PITCH { s: RegRef },
}
