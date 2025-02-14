use rusty_chips::compiler::{assembler::Assembler, program::Program};
use std::rc::Rc;

fn main() {
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
	asm.compile().map_err(|e| e.to_string()).unwrap();

	print!("{}", prog);
}
