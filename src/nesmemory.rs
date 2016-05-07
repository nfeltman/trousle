
pub trait MemorySystem {
    fn readByte(&mut self, u16) -> u8;
    fn writeByte(&mut self, u16, u8) -> ();
}

pub struct MusicSystem {
    ram : [u8;2048],
}

impl MemorySystem for MusicSystem {
    fn readByte(&mut self, addr : u16) -> u8 {
    	self.ram[addr as usize]
    }

    fn writeByte(&mut self, addr : u16, val : u8) -> () {
    	self.ram[addr as usize] = val;
    }
}
