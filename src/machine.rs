use crate::compiler::program::Program;

pub const MEMORY_SIZE: usize = 4096;
const REGISTER_COUNT: usize = 16;
const FRAME_WIDTH_BITS: usize = 128;
const FRAME_HEIGHT_BITS: usize = 64;
const STACK_SIZE: usize = 16;
const BIT_PLANES: usize = 2;

const FRAME_WIDTH_BYTES: usize = FRAME_WIDTH_BITS / 8;
const FRAME_HEIGHT_BYTES: usize = FRAME_HEIGHT_BITS / 8;

pub type Memory = [u8; MEMORY_SIZE];

pub type BitPlane = [u8; FRAME_WIDTH_BYTES * FRAME_HEIGHT_BYTES];

pub struct VirtualMachine {
	pub mem: Memory,
	pub pc: u16,
	pub stack: [u16; STACK_SIZE],
	pub sp: usize,
	pub idx: u16,
	pub reg: [u8; REGISTER_COUNT],
	pub tim_delay: u8,
	pub tim_sound: u8,
	pub frame: [BitPlane; BIT_PLANES],
}

impl VirtualMachine {
	pub fn new() -> Self {
		Self {
			mem: [0; MEMORY_SIZE],
			pc: 0x200,
			stack: [0; STACK_SIZE],
			sp: 0,
			idx: 0,
			reg: [0; REGISTER_COUNT],
			tim_delay: 0,
			tim_sound: 0,
			frame: [[0; FRAME_WIDTH_BYTES * FRAME_HEIGHT_BYTES]; BIT_PLANES],
		}
	}

	pub fn load_program(&mut self, program: &Program) {
		self.mem[..program.data().len()].copy_from_slice(program.data());
	}
}
