use std::io::*;

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

pub enum VideoStandard {
	NTSC, PAL, Both
}

fn read_nsf_header<T : Read> (mut reader : T)-> Result<NsfHeader> {

	let mut buff : [u8; 128] = [0; 128];
	match reader.read(&mut buff) {
		Err(e) => return Err(e),
		Ok(n) if n < 128 => return Err(Error::new(ErrorKind::UnexpectedEof, "Expected at least 128 bytes for NSF header.")),
		_ => {} 
	}

	// first five bytes must be: 4e 45 53 4d 1a
	assert!(buff[0] == 0x4e);
	assert!(buff[1] == 0x45);
	assert!(buff[2] == 0x53);
	assert!(buff[3] == 0x4d);
	assert!(buff[4] == 0x1a);

	Ok(NsfHeader{
		num_songs : 0,
		start_song : 0,
		load_address : 0,
		init_address : 0,
		play_address : 0,
		play_speed_pal : 0,
		play_speed_ntsc : 0,
		standard : VideoStandard::NTSC,
	})
}