use libc::STDIN_FILENO;
use std::ffi::CString;
use std::mem;
use std::os::raw::c_char;

/*** libc? ***/

fn errno() -> i32 {
    std::io::Error::last_os_error().raw_os_error().unwrap()
}

/*** "defines" ***/

fn ctrl_key(k: char) -> c_char {
    k as i8 & 0x1f
}

/*** data ***/

static mut ORIG_TERMIOS: libc::termios = unsafe { mem::zeroed() };

/*** terminal ***/

fn die(s: &str) {
    let c_str = CString::new(s).unwrap();
    unsafe {
        libc::perror(c_str.as_ptr() as *const c_char);
        libc::exit(1);
    }
}

extern "C" fn disable_raw_mode() {
    if unsafe { libc::tcsetattr(STDIN_FILENO, libc::TCSAFLUSH, &raw mut ORIG_TERMIOS) } != 0 {
        die("tcgetattr");
    }
}

fn enable_raw_mode() {
    if unsafe { libc::tcgetattr(libc::STDIN_FILENO, &raw mut ORIG_TERMIOS) } == -1 {}
    unsafe { libc::atexit(disable_raw_mode) };

    let mut raw = unsafe { ORIG_TERMIOS };

    use libc::{
        BRKINT, CS8, ECHO, ICANON, ICRNL, IEXTEN, INPCK, ISIG, ISTRIP, IXON, OPOST, VMIN, VTIME,
    };
    raw.c_iflag &= !(BRKINT | ICRNL | INPCK | ISTRIP | IXON);
    raw.c_oflag &= !(OPOST);
    raw.c_cflag |= CS8;
    raw.c_lflag &= !(ECHO | ICANON | IEXTEN | ISIG);
    raw.c_cc[VMIN] = 0;
    raw.c_cc[VTIME] = 1;

    if unsafe { libc::tcsetattr(libc::STDIN_FILENO, libc::TCSAFLUSH, &mut raw) } != 0 {
        die("tcgetattr");
    }
}

fn main() {
    enable_raw_mode();

    loop {
        let mut c: c_char = 0;
        if unsafe { libc::read(STDIN_FILENO, mem::transmute(&mut c), 1) } == -1
            && errno() != libc::EAGAIN
        {
            die("read");
        }
        if unsafe { libc::iscntrl(c.into()) } != 0 {
            print!("{c}\r\n");
        } else {
            print!("{c} ('{}')\r\n", c as u8 as char);
        }

        if c == ctrl_key('q') {
            break;
        }
    }
}
