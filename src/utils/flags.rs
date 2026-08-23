pub struct Flags(pub u32);
pub const FLAG_DEBUG: u32 = 1 << 0;
pub const FLAG_NO_COLOR: u32 = 1 << 1;
pub const FLAG_SHOW_OUTPUT: u32 = 1 << 2;
pub const FLAG_AST: u32 = 1 << 3;

pub static mut FLAGS: Flags = Flags(0);
