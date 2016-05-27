
pub trait MemorySystem {
    fn readByte(&mut self, u16) -> u8;
    fn writeByte(&mut self, u16, u8) -> ();
    fn getRef(&mut self, u16) -> &u8;

    fn readWord(&mut self, addr : u16) -> u16 {
    	(self.readByte(addr) as u16) + (self.readByte(addr+1) as u16) * 256
    }
    //fn writeWord(&mut self, u16, u16) -> ();
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

    fn getRef(&mut self, addr : u16) -> &u8 {
    	&self.ram[addr as usize];
    }
}
