const MEMORY_SIZE: usize = 4096;
const REGISTER_COUNT: usize = 16;
const FRAME_WIDTH_BITS: usize = 64;
const FRAME_HEIGHT_BITS: usize = 32;

const FRAME_WIDTH_BYTES: usize = FRAME_WIDTH_BITS / 8;
const FRAME_HEIGHT_BYTES: usize = FRAME_HEIGHT_BITS / 8;

pub type Memory = [u8; MEMORY_SIZE];

pub struct VirtualMachine {
	pub mem: Memory,
	pub reg: [u8; REGISTER_COUNT],
	pub idx: u16,
	pub sp: u8,
	pub tim_delay: u8,
	pub tim_sound: u8,
	pub frame: [u8; FRAME_WIDTH_BYTES * FRAME_HEIGHT_BYTES],
}
