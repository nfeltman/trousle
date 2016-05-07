use std::io::*;

#[derive(Debug)]
pub struct NsfHeader {
	num_songs : u8,
	start_song : u8,
	
	load_address : u16,
	init_address : u16,
	play_address : u16,

	play_speed_ntsc : u16,
	play_speed_pal : u16,

	standard : VideoStandard,
}

#[derive(Debug)]
pub enum VideoStandard {
	NTSC, PAL, Both
}

pub fn read_nsf_header<T : Read> (mut reader : T)-> Result<NsfHeader> {

	// read into a buffer of 128 bytes; if that didn't work, error out
	let mut buff : [u8; 128] = [0; 128];
	match reader.read(&mut buff) {
		Err(e) => return Err(e),
		Ok(n) if n < 128 => return Err(Error::new(ErrorKind::UnexpectedEof, "Expected at least 128 bytes for NSF header.")),
		_ => {} 
	}

	// function to read a word (2 bytes) from the buffer
	let word = |i : usize| buff[i] as u16 + (buff[i+1] as u16 * 256);

	// first five bytes must be: 4e 45 53 4d 1a
	assert!(buff[0] == 0x4e);
	assert!(buff[1] == 0x45);
	assert!(buff[2] == 0x53);
	assert!(buff[3] == 0x4d);
	assert!(buff[4] == 0x1a);

	// version number must be 1
	assert!(buff[5] == 1);

	let standard_byte = match buff[0x7A]{
		0 => VideoStandard::NTSC,
		1 => VideoStandard::PAL,
		2 | 3 => VideoStandard::Both,
		_ => return Err(Error::new(ErrorKind::UnexpectedEof, "PAL/NTSC byte was malformed."))
	};

	Ok(NsfHeader{
		num_songs : buff[0x6],
		start_song : buff[0x7],
		load_address : word(0x8),
		init_address : word(0xA),
		play_address : word(0xC),
		play_speed_ntsc : word(0x6E),
		play_speed_pal : word(0x78),
		standard : standard_byte,
	})
}