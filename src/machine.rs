use super::instructions::*;
use nesmemory::MemorySystem;

pub struct ProcessorState {
    status : u8,
    a : u8,
    x : u8,
    y : u8,
    sp : u8, // stack pointer
    pc : u16
}



fn step<M : MemorySystem>(mut s : ProcessorState, m : M) -> Result<(),()> {
	use super::instructions::Simple6502::*;
	use super::instructions::Simple6502::Instruction as I;
	
	fn flag_mask (x : Flags) { match x{
		Flags::Carry    => 0b_0000_0001,
		Flags::Zero 	=> 0b_0000_0010,
		Flags::Interupt => 0b_0000_0100,
		Flags::Decimal  => 0b_0000_1000,
		Flags::Overflow => 0b_0100_0000,
		Flags::Negative => 0b_1000_0000,
	}};

	fn stackAddr(addr : u8) -> u16 {
		addr as u16 + 0x0100_u16
	};

	fn le(b1 : u8, b2 : u8) -> u16 {
		b2 as u16 * 256 + b1
	};

	let nextByte = || m.readByte(s.pc+1);
	let nextBytes = || (m.readByte(s.pc+1), m.readByte(s.pc+2));
	let nextWord = || {let (b1,b2) = nextBytes(); le (b1,b2)};

	let inst = simplify(Full6502::parse_instruction(m.readByte(s.pc)));
	match inst {
		I::Invalid => Err (()),
		I::CommonOp (op) => {
			use super::instructions::CommonOps as Co;
			match op {
				Co::NoOp => {s.pc+=1; Ok(())},
				Co::Break |	Co::JumpToSubroutine | Co::ReturnFromSubroutine | Co::ReturnFromInterupt => Err(()),
				Co::Jump => {s.pc= nextWord(); Ok(())},
				Co::JumpIndirect => {
					let (b1,b2) = nextBytes();
					s.pc = le(m.readByte(le(b1,b2)), m.readByte(le(b1+1,b2)));
					Ok(())
				},
				Co::Push(reg) => {
					m.writeByte(stackAddr(s.sp),
					match reg {
						AccOrStat::Acc => s.a,
						AccOrStat::Stat => s.status
					});
					s.sp-=1;
					s.pc+=1;
					Ok(())
				},
				Co::Pull(reg) => {
					let r = match reg {
						AccOrStat::Acc => &s.a,
						AccOrStat::Stat => &s.status
					};
					*r = m.readByte(stackAddr(s.sp));
					s.sp+=1;
					s.pc+=1;
					Ok(())
				},
			}
		}
		I::Unary (UnOp, UnArg) => (),
		I::Binary (BinOp, Register, BinArg) => (),
		I::Store (Register, StoreLoc) => (),
		I::Branch (SetOrClear, Flags) => (),
		I::SetFlag (sc, f) => { 
			let mask = flag_mask(f);
			match sc {
			    SetOrClear::Set => {s.status |= mask}
			    SetOrClear::Clear => {s.status &= 0xFF ^ mask}
			};
			s.pc+=1;
			Ok(())
		},
	}
}
