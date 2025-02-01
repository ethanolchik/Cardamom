pub struct Flags(pub u32);
pub const FLAG_DEBUG: u32 = 1 << 0;
pub const FLAG_NO_COLOR: u32 = 1 << 1;

pub static mut FLAGS: Flags = Flags(0);