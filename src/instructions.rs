pub enum CommonOps {
	NoOp,
	Break,
	JumpToSubroutine,
	ReturnFromSubroutine,
	ReturnFromInterupt,
	Jump,
	JumpIndirect,
	Push(AccOrStat),
	Pull(AccOrStat),
}

// either accumulator or status, for push/pop instructions
pub enum AccOrStat{ Acc, Stat }

// for flag operations
pub enum SetOrClear { Set, Clear}


pub mod Full6502 {

	pub enum Instruction {
		Invalid,
		CommonOp (super::CommonOps),
		TestBits (TestBitsAM),
		IncDecInst (IncDecOp, IncDecAM),
		AccInst (AccOp, AccAM),
		ShiftInst (ShiftOp, ShiftAM),
		CompareXY (XorY, CompAM),
		StoreXY (XorY, StoreAM),
		LoadXY (XorY, LoadAM),
		Branch (super::SetOrClear, BranchableFlags),
		SetFlag (super::SetOrClear, SetableFlags),
		Transfer (TransferRegs, TransferDir),
	}

	// options for flag set/clear/branch
	pub enum BranchableFlags {
		Negative,
		Zero,
		Carry,
		Overflow
	}
	pub enum SetableFlags {	
		Carry,
		Interupt,
		Decimal,
		Overflow 
	}

	// operations for accumulator 
	pub enum AccOp {
		Or,
		And,
		Exor,
		Add,
		Store,
		Load,
		Comp,
		Sub
	}

	// address modes for accumulator operations
	pub enum AccAM {
		Immediate,
		ZeroPage,
		ZeroPageX,
		Absolute,
		AbsoluteX,
		AbsoluteY,
		IndirectX,
		IndirectY,
	}

	// various shift operations
	pub enum ShiftOp {
		ShiftLeft,
		RotateLeft,
		ShiftRight,
		RotateRight,
	}

	// address modes for the shift operations
	pub enum ShiftAM {
		Acc,
		ZeroPage,
		ZeroPageX,
		Absolute,
		AbsoluteX
	}

	// increment and decrement operations
	pub enum IncDecOp {
		Increment,
		Decrement
	}

	// address modes for incremenet/decrement
	pub enum IncDecAM {
		X,
		Y,
		ZeroPage,
		ZeroPageX,
		Absolute,
		AbsoluteX
	}

	pub enum XorY { X, Y}

	// address modes for comp xy operations
	pub enum CompAM {
		Immediate,
		ZeroPage,
		Absolute,
	}

	// address modes for load to X/Y
	pub enum LoadAM {
		Immediate,
		ZeroPage,
		ZeroPageIndirect,
		Absolute,
		AbsoluteIndirect
	}

	// address modes for store to X/Y
	pub enum StoreAM {
		ZeroPage,
		ZeroPageIndirect,
		Absolute
	}

	// address modes for test bits
	pub enum TestBitsAM {
		ZeroPage,
		Absolute
	}

	// transfer operation registers and directions
	pub enum TransferDir { Forward, Backward}
	pub enum TransferRegs{ XtoA, YtoA, XtoS }

	pub fn parse_instruction(b : u8) -> Instruction
	{
		use self::Instruction::* ;
		use super::CommonOps::* ;
		use super::AccOrStat ;
		use super::SetOrClear ;
		if b & 0b0011 == 0b0001 {
			AccInst(
				match b >> 5 {
					0 => AccOp::Or,
					1 => AccOp::And,
					2 => AccOp::Exor,
					3 => AccOp::Add,
				 	4 => AccOp::Store,
				 	5 => AccOp::Load,
				 	6 => AccOp::Comp,
				 	7 => AccOp::Sub, 
				 	i => panic!("Match not exhaustive?!  Somehow got {:?}. ", i) },
				match (b >> 2) & 0b0111 {
				    0 => AccAM::IndirectX, 
				    1 => AccAM::ZeroPage, 
				    2 => AccAM::Immediate, 
				    3 => AccAM::Absolute, 
				    4 => AccAM::IndirectY, 
				    5 => AccAM::ZeroPageX, 
				    6 => AccAM::AbsoluteY, 
				    7 => AccAM::AbsoluteX, 
				 	i => panic!("Match not exhaustive?!  Somehow got {:?}. ", i) })
		} else if (b & 0b10000011) == 0b00000010 {
			let operation = 
				match (b>>5) & 0b011 {
					0 => ShiftOp::ShiftLeft,
					1 => ShiftOp::RotateLeft,
					2 => ShiftOp::ShiftRight,
					3 => ShiftOp::RotateRight,
				 	i => panic!("Match not exhaustive?!  Somehow got {:?}. ", i) };
			match (b >> 2) & 0b0111 {
			    1 => ShiftInst(operation, ShiftAM::ZeroPage), 
			    2 => ShiftInst(operation, ShiftAM::Acc), 
			    3 => ShiftInst(operation, ShiftAM::Absolute), 
			    5 => ShiftInst(operation, ShiftAM::ZeroPageX), 
			    7 => ShiftInst(operation, ShiftAM::AbsoluteX), 
			    0|4|6 => Invalid, 
			 	i => panic!("Match not exhaustive?!  Somehow got {:?}. ", i) }
		}
		else if (b & 0b_11111) == 0b_10000
		{
			Branch(
				match (b >> 5) & 0b01 {
				    0 => SetOrClear::Clear, 
				    1 => SetOrClear::Set, 
				 	i => panic!("Match not exhaustive?!  Somehow got {:?}. ", i) },
				match b >> 6 {
				    0 => BranchableFlags::Negative, 
				    1 => BranchableFlags::Overflow, 
				    2 => BranchableFlags::Carry, 
				    3 => BranchableFlags::Zero, 
				 	i => panic!("Match not exhaustive?!  Somehow got {:?}. ", i) })
		}
		else if (b & 0b_1110_0101) == 0b_1010_0100
		{
			LoadXY(
			match (b >> 1) & 1 {
			    0 => XorY::Y,
			    1 => XorY::X,
			 	i => panic!("Match not exhaustive?!  Somehow got {:?}. ", i) },
			 match (b >> 3) & 0b011 {
			    0 => LoadAM::ZeroPage,
			    1 => LoadAM::Absolute,
			    2 => LoadAM::ZeroPageIndirect,
			    3 => LoadAM::AbsoluteIndirect,
			 	i => panic!("Match not exhaustive?!  Somehow got {:?}. ", i) },
				)
		}
		else if (b & 0b_1100_0111) == 0b_1100_0110
		{
			IncDecInst(
			match (b >> 3) & 1 {
			    0 => IncDecOp::Decrement,
			    1 => IncDecOp::Increment,
			 	i => panic!("Match not exhaustive?!  Somehow got {:?}. ", i) },
			match (b >> 3) & 0b011 {
			    0 => IncDecAM::ZeroPage,
			    1 => IncDecAM::Absolute,
			    2 => IncDecAM::ZeroPageX,
			    3 => IncDecAM::AbsoluteX,
			 	i => panic!("Match not exhaustive?!  Somehow got {:?}. ", i) },
			)
		}
		else {
		    match b {
			    0x00 => CommonOp(Break), 
			    0x20 => CommonOp(JumpToSubroutine), 
			    0x40 => CommonOp(ReturnFromInterupt), 
			    0x60 => CommonOp(ReturnFromSubroutine), 
			    0xA0 => LoadXY(XorY::Y, LoadAM::Immediate), 
			    0xC0 => CompareXY(XorY::Y, CompAM::Immediate), 
			    0xE0 => CompareXY(XorY::X, CompAM::Immediate), 
			    0x08 => CommonOp(Push(AccOrStat::Stat)), 
			    0x18 => SetFlag(SetOrClear::Clear,SetableFlags::Carry), 
			    0x28 => CommonOp(Pull(AccOrStat::Stat)), 
			    0x38 => SetFlag(SetOrClear::Set,SetableFlags::Carry), 
			    0x48 => CommonOp(Push(AccOrStat::Acc)), 
			    0x58 => SetFlag(SetOrClear::Clear,SetableFlags::Interupt), 
			    0x68 => CommonOp(Pull(AccOrStat::Acc)), 
			    0x78 => SetFlag (SetOrClear::Set,SetableFlags::Interupt), 
			    0x88 => IncDecInst (IncDecOp::Decrement, IncDecAM::Y), 
			    0x98 => Transfer (TransferRegs::YtoA,TransferDir::Forward), 
			    0xA8 => Transfer (TransferRegs::YtoA,TransferDir::Backward), 
			    0xB8 => SetFlag (SetOrClear::Clear,SetableFlags::Overflow), 
			    0xC8 => IncDecInst (IncDecOp::Increment, IncDecAM::Y), 
			    0xD8 => SetFlag (SetOrClear::Clear,SetableFlags::Decimal), 
			    0xE8 => IncDecInst (IncDecOp::Increment, IncDecAM::X), 
			    0xF8 => SetFlag (SetOrClear::Set,SetableFlags::Decimal), 
		    	0xC4 => CompareXY(XorY::Y, CompAM::ZeroPage),
		    	0xCC => CompareXY(XorY::Y, CompAM::Absolute),
		    	0xE4 => CompareXY(XorY::X, CompAM::ZeroPage),
		    	0xEC => CompareXY(XorY::X, CompAM::Absolute),
		    	0xA2 => LoadXY(XorY::X, LoadAM::Immediate),
		    	0x24 => TestBits(TestBitsAM::ZeroPage),
		    	0x2C => TestBits(TestBitsAM::Absolute),
		    	0x4C => CommonOp(Jump),
		    	0x6C => CommonOp(JumpIndirect),
			    0x8A => Transfer(TransferRegs::XtoA,TransferDir::Forward), 
			    0x9A => Transfer(TransferRegs::XtoS,TransferDir::Forward), 
			    0xAA => Transfer(TransferRegs::XtoA,TransferDir::Backward), 
			    0xAB => Transfer(TransferRegs::XtoS,TransferDir::Backward), 
			    0xCA => IncDecInst (IncDecOp::Decrement, IncDecAM::X), 
			    0xEA => CommonOp(NoOp), 
			    0x84 => StoreXY (XorY::Y, StoreAM::ZeroPage), 
			    0x86 => StoreXY (XorY::X, StoreAM::ZeroPage),
			    0x8C => StoreXY (XorY::Y, StoreAM::Absolute), 
			    0x8E => StoreXY (XorY::X, StoreAM::Absolute), 
			    0x94 => StoreXY (XorY::Y, StoreAM::ZeroPageIndirect), 
			    0x96 => StoreXY (XorY::X, StoreAM::ZeroPageIndirect),
		    	_ => Invalid
		    }
		}
	}
}

mod Simple6502 {
	use super::*;

	pub enum Instruction {
		Invalid,
		CommonOp (super::CommonOps),
		Unary (UnOp, UnArg),
		Binary (BinOp, Register, BinArg),
		Store (Register, StoreLoc),
		Branch (SetOrClear, Flags),
		SetFlag (SetOrClear, Flags),
	}

	pub enum Flags {
		Negative,
		Zero,
		Carry,
		Overflow,
		Interupt,
		Decimal,
	}

	// binary operations 
	pub enum BinOp {
		Set,
		Or,
		And,
		Exor,
		Add,
		Comp,
		Sub,
		TestBits
	}

	// address modes for binary operations
	pub enum BinArg {
		Immediate,
		ZeroPage,
		ZeroPageX,
		ZeroPageY,
		Absolute,
		AbsoluteX,
		AbsoluteY,
		IndirectX,
		IndirectY,
		Reg(Register)
	}

	// store locations
	pub enum StoreLoc {
		ZeroPage,
		ZeroPageX,
		ZeroPageY,
		Absolute,
		AbsoluteX,
		AbsoluteY,
		IndirectX,
		IndirectY
	}

	pub enum Register {
		X, Y, A, Stack
	}

	// various unary operations
	pub enum UnOp {
		ShiftLeft,
		RotateLeft,
		ShiftRight,
		RotateRight,
		Increment,
		Decrement
	}

	// address modes for the unary operations
	pub enum UnArg {
		ZeroPage,
		ZeroPageX,
		Absolute,
		AbsoluteX,
		Reg(Register)
	}


	pub fn simplify(i : Full6502::Instruction) -> Instruction
	{
		use self::Instruction::*;
		use super::Full6502 as Full;
		use super::Full6502::Instruction as FullI;
		match i {
			FullI::Invalid => Invalid,
			FullI::CommonOp (op) => CommonOp(op),
			FullI::TestBits (am) => Binary(BinOp::TestBits, Register::A,
				match am {
					Full::TestBitsAM::ZeroPage => BinArg::ZeroPage, 
					Full::TestBitsAM::Absolute => BinArg::Absolute}),
			FullI::IncDecInst (op, am) => Unary(
				match op {
					Full::IncDecOp::Increment => UnOp::Increment,
					Full::IncDecOp::Decrement => UnOp::Decrement,
				},
				match am {
					Full::IncDecAM::X => UnArg::Reg(Register::X),
					Full::IncDecAM::Y => UnArg::Reg(Register::Y),
					Full::IncDecAM::ZeroPage => UnArg::ZeroPage,
					Full::IncDecAM::ZeroPageX => UnArg::ZeroPageX,
					Full::IncDecAM::Absolute => UnArg::Absolute,
					Full::IncDecAM::AbsoluteX => UnArg::AbsoluteX
				}),
			FullI::AccInst (op, am) => {
				let arg = match am {
					Full::AccAM::Immediate	=> BinArg::Immediate,
					Full::AccAM::ZeroPage	=> BinArg::ZeroPage,
					Full::AccAM::ZeroPageX	=> BinArg::ZeroPageX,
					Full::AccAM::Absolute	=> BinArg::Absolute,
					Full::AccAM::AbsoluteX	=> BinArg::AbsoluteX,
					Full::AccAM::AbsoluteY	=> BinArg::AbsoluteY,
					Full::AccAM::IndirectX	=> BinArg::IndirectX,
					Full::AccAM::IndirectY	=> BinArg::IndirectY,
				};
				match op {
					Full::AccOp::Or 	=> Binary(BinOp::Or, Register::A, arg),
					Full::AccOp::And 	=> Binary(BinOp::And, Register::A, arg),
					Full::AccOp::Exor 	=> Binary(BinOp::Exor, Register::A, arg),
					Full::AccOp::Add 	=> Binary(BinOp::Add, Register::A, arg),
					Full::AccOp::Store	=> Store(Register::A, 
						match am {
							Full::AccAM::Immediate	=> panic!("Store into immediate isn't a real instruction."),
							Full::AccAM::ZeroPage	=> StoreLoc::ZeroPage,
							Full::AccAM::ZeroPageX	=> StoreLoc::ZeroPageX,
							Full::AccAM::Absolute	=> StoreLoc::Absolute,
							Full::AccAM::AbsoluteX	=> StoreLoc::AbsoluteX,
							Full::AccAM::AbsoluteY	=> StoreLoc::AbsoluteY,
							Full::AccAM::IndirectX	=> StoreLoc::IndirectX,
							Full::AccAM::IndirectY	=> StoreLoc::IndirectY,
						}),
					Full::AccOp::Load 	=> Binary(BinOp::Set, Register::A, arg),
					Full::AccOp::Comp 	=> Binary(BinOp::Comp, Register::A, arg),
					Full::AccOp::Sub 	=> Binary(BinOp::Sub, Register::A, arg),
				}},
			FullI::ShiftInst (op, am) => Unary(
				match op {
					Full::ShiftOp::ShiftLeft	=> UnOp::ShiftLeft,
					Full::ShiftOp::RotateLeft	=> UnOp::RotateLeft,
					Full::ShiftOp::ShiftRight	=> UnOp::ShiftRight,
					Full::ShiftOp::RotateRight	=> UnOp::RotateRight,
				},
				match am {
					Full::ShiftAM::Acc			=> UnArg::Reg(Register::A),
					Full::ShiftAM::ZeroPage		=> UnArg::ZeroPage,
					Full::ShiftAM::ZeroPageX	=> UnArg::ZeroPageX,
					Full::ShiftAM::Absolute		=> UnArg::Absolute,
					Full::ShiftAM::AbsoluteX	=> UnArg::AbsoluteX,
				}),
			FullI::CompareXY (reg, am) => Binary( BinOp::Comp,
				match reg {
					Full::XorY::X => Register::X,
					Full::XorY::Y => Register::Y,
				},
				match am {
					Full::CompAM::Immediate	=> BinArg::Immediate,
					Full::CompAM::ZeroPage	=> BinArg::ZeroPage,
					Full::CompAM::Absolute	=> BinArg::Absolute,
				}),
			FullI::StoreXY (reg, am) => Store(
				match reg {
					Full::XorY::X => Register::X,
					Full::XorY::Y => Register::Y,
				},
				match am {
					Full::StoreAM::ZeroPage			=> StoreLoc::ZeroPage,
					Full::StoreAM::ZeroPageIndirect	=> 
						match reg {
							Full::XorY::X => StoreLoc::ZeroPageY,
							Full::XorY::Y => StoreLoc::ZeroPageX,
						},
					Full::StoreAM::Absolute			=> StoreLoc::Absolute,
				}),
			FullI::LoadXY (reg, am) => Binary( BinOp::Set,
				match reg {
					Full::XorY::X => Register::X,
					Full::XorY::Y => Register::Y,
				},
				match am {
					Full::LoadAM::Immediate			=> BinArg::Immediate,
					Full::LoadAM::ZeroPage			=> BinArg::ZeroPage,
					Full::LoadAM::ZeroPageIndirect	=>
						match reg {
							Full::XorY::X => BinArg::ZeroPageY,
							Full::XorY::Y => BinArg::ZeroPageX,
						},
					Full::LoadAM::Absolute			=> BinArg::Absolute,
					Full::LoadAM::AbsoluteIndirect	=>
						match reg {
							Full::XorY::X => BinArg::AbsoluteY,
							Full::XorY::Y => BinArg::AbsoluteX,
						},
				}),
			FullI::Branch (soc, flag) => Branch(soc,
				match flag{
					Full::BranchableFlags::Negative => Flags::Negative,
					Full::BranchableFlags::Zero 	=> Flags::Zero,
					Full::BranchableFlags::Carry 	=> Flags::Carry,
					Full::BranchableFlags::Overflow => Flags::Overflow,
				}),
			FullI::SetFlag (soc, flag) => SetFlag(soc,
				match flag{
					Full::SetableFlags::Interupt 	=> Flags::Interupt,
					Full::SetableFlags::Decimal		=> Flags::Decimal,
					Full::SetableFlags::Carry 		=> Flags::Carry,
					Full::SetableFlags::Overflow 	=> Flags::Overflow,
				}),
			FullI::Transfer (regs, dir) => {
				let (alpha, beta) = match regs {
					Full::TransferRegs::XtoA => (Register::X, Register::A), 
					Full::TransferRegs::YtoA => (Register::Y, Register::A), 
					Full::TransferRegs::XtoS => (Register::X, Register::Stack)
				};
				match dir {
				    Full::TransferDir::Forward  => Binary(BinOp::Set, beta, BinArg::Reg(alpha)),
				    Full::TransferDir::Backward => Binary(BinOp::Set, alpha, BinArg::Reg(beta)),
				}
			},
		}
	}
}



