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

	fn firstPage(addr : u8) -> u16 {
		addr as u16 + 0x0100_u16
	};

	fn le(b1 : u8, b2 : u8) -> u16 {
		b2 as u16 * 256 + b1
	};

	let grabByte = || {let b = m.readByte(s.pc); s.pc+=1; b};
	let grabBytes = || {let (b1,b2) = (m.readByte(s.pc), m.readByte(s.pc+1)); (b1,b2)};
	let grabWord = || {let (b1,b2) = grabBytes(); le (b1,b2)};

	let registerLoc = |r : Register| {
			match r {
				Register::X => &s.x,
				Register::Y => &s.y,
				Register::A => &s.a,
				Register::Stack => &s.sp,
			}};

	let memoryAddr = |r : MemLoc| {
			match r {
				MemLoc::ZeroPage 	=> grabByte() as u16,
				MemLoc::ZeroPageX 	=> (grabByte() + s.x) as u16,
				MemLoc::ZeroPageY 	=> (grabByte() + s.y) as u16,
				MemLoc::Absolute 	=> grabWord(),
				MemLoc::AbsoluteX 	=> grabWord() + s.x as u16,
				MemLoc::AbsoluteY 	=> grabWord() + s.y as u16,
				MemLoc::IndirectX 	=> m.readWord((grabByte() + s.x) as u16),
				MemLoc::IndirectY 	=> m.readWord(grabByte() as u16) + (s.y as u16),
			}};

	match Full6502::parse_instruction(grabByte()).simplify() {
		I::Invalid => Err (()),
		I::CommonOp (op) => {
			use super::instructions::CommonOps as Co;
			match op {
				Co::NoOp => Ok(()),
				Co::Break |	Co::JumpToSubroutine | Co::ReturnFromSubroutine | Co::ReturnFromInterupt => Err(()),
				Co::Jump => {s.pc = grabWord(); Ok(())},
				Co::JumpIndirect => {
					let (b1,b2) = grabBytes();
					s.pc = le(m.readByte(le(b1,b2)), m.readByte(le(b1+1,b2)));
					Ok(())
				},
				Co::Push(reg) => {
					m.writeByte(firstPage(s.sp),
					match reg {
						AccOrStat::Acc => s.a,
						AccOrStat::Stat => s.status
					});
					s.sp-=1;
					Ok(())
				},
				Co::Pull(reg) => {
					let r = match reg {
						AccOrStat::Acc => &s.a,
						AccOrStat::Stat => &s.status
					};
					*r = m.readByte(firstPage(s.sp));
					s.sp+=1;
					Ok(())
				},
			}
		}
		I::Unary (op, arg) => {
			let spot = match arg {
				UnArg::Reg(r) => registerLoc (r),
				UnArg::Mem(r) => m.getRef(memoryAddr (r)),
			};
			match op {
			    UnOp::ShiftLeft => {
			    	s.status = s.status & 0xFE + (*spot >> 7);
			    	*spot <<= 1
			    },
				UnOp::RotateLeft => {
					let carrybit = s.status & 1;
			    	s.status = s.status & 0xFE + (*spot >> 7);
			    	*spot = (*spot << 7) + carrybit
			    },
				UnOp::ShiftRight => {
			    	s.status = s.status & 0xFE + (*spot & 1);
			    	*spot >>= 1
			    },
				UnOp::RotateRight => {
					let carrybit = s.status << 7;
			    	s.status = s.status & 0xFE + (*spot & 1);
			    	*spot = (spot >> 1) + carrybit;
			    },
				UnOp::Increment => *spot += 1,
				UnOp::Decrement => *spot -= 1,
			}

			// handle flags

			Ok(())
		},
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
