use libc::{STDIN_FILENO, STDOUT_FILENO};
use std::ffi::CString;
use std::io::{self, Read, Write};
use std::mem;
use std::os::raw::c_char;

/*** "defines" ***/

const VERSION: &str = "0.0.1";

fn ctrl_key(k: char) -> c_char {
    k as i8 & 0x1f
}

#[derive(Clone, Copy, PartialEq)]
enum Key {
    ArrowLeft = 1000,
    ArrowRight,
    ArrowUp,
    ArrowDown,
    Del,
    Home,
    End,
    PageUp,
    PageDown,
}

/*** libc? ***/

fn errno() -> i32 {
    std::io::Error::last_os_error().raw_os_error().unwrap()
}

fn stdout_write(b: &[u8]) -> io::Result<()> {
    let mut stdout = io::stdout();
    stdout.write(b)?;
    stdout.flush()
}

fn stdin_read_byte() -> io::Result<u8> {
    let mut b = [0; 1];
    io::stdin().read_exact(&mut b)?;
    Ok(b[0])
}

fn get_cursor_position() -> Option<Screen> {
    stdout_write(b"\x1b[6n").ok()?;
    print!("\r\n");

    let mut buf: [c_char; 32] = [0; 32];
    let mut i = 0;
    while i < mem::size_of::<[c_char; 32]>() - 1 {
        buf[i] = match stdin_read_byte() {
            Ok(b) => b as c_char,
            Err(_) => break,
        };
        if buf[i] as u8 == b'R' {
            break;
        }
        i += 1;
    }
    buf[i] = 0;

    if buf[0] as u8 != b'\x1b' || buf[1] as u8 != b'[' {
        return None;
    }
    let c_str = CString::new("%d;%d").unwrap();
    let mut rows = 0;
    let mut cols = 0;
    // left scanf because it's simpler/smaller than
    // the Rust alternative
    if unsafe { libc::sscanf(&buf[2], c_str.as_ptr(), &mut rows, &mut cols) } == 2 {
        return Some(Screen { rows, cols });
    }

    None
}

fn get_window_size() -> Option<Screen> {
    let mut ws: libc::winsize = unsafe { mem::zeroed() };

    if unsafe { libc::ioctl(STDOUT_FILENO, libc::TIOCGWINSZ, &mut ws) } == -1 || ws.ws_col == 0 {
        stdout_write(b"\x1b[999C\x1b[999B").ok()?;

        return get_cursor_position();
    }

    Some(Screen {
        rows: ws.ws_row as usize,
        cols: ws.ws_col as usize,
    })
}

/*** data ***/

static mut ORIG_TERMIOS: libc::termios = unsafe { mem::zeroed() };

/*** terminal ***/

fn die(fn_name: &str) -> ! {
    let _ = stdout_write(b"\x1b[2J");
    let _ = stdout_write(b"\x1b[H");

    let c_str = CString::new(fn_name).unwrap();
    unsafe {
        libc::perror(c_str.as_ptr() as *const c_char);
        std::process::exit(1);
    }
}

extern "C" fn disable_raw_mode() {
    if unsafe { libc::tcsetattr(STDIN_FILENO, libc::TCSAFLUSH, &raw mut ORIG_TERMIOS) } != 0 {
        die("tcsetattr");
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
        die("tcsetattr");
    }
}

#[derive(Debug, Default)]
struct Cursor {
    x: usize,
    y: usize,
}

#[derive(Debug, Default)]
struct Screen {
    rows: usize,
    cols: usize,
}

#[derive(Debug)]
struct Editor {
    cursor: Cursor,
    screen: Screen,
}

impl Editor {
    fn new() -> Self {
        let cursor = Cursor::default();
        let screen = get_window_size().unwrap_or_else(|| die("get_window_size"));

        Self { cursor, screen }
    }

    fn draw_rows(&mut self, buf: &mut String) {
        for y in 0..self.screen.rows {
            if y == self.screen.rows / 3 {
                let welcome = format!("Kilo editor -- version {VERSION}");
                let len = std::cmp::min(welcome.len(), self.screen.cols);
                let mut padding = (self.screen.cols - len) / 2;
                if padding > 0 {
                    buf.push('~');
                    padding -= 1;
                }
                while padding != 0 {
                    buf.push(' ');
                    padding -= 1;
                }
                for c in welcome.chars().take(len) {
                    buf.push(c);
                }
            } else {
                buf.push('~');
            }

            buf.push_str("\x1b[K");
            if y < self.screen.rows - 1 {
                buf.push_str("\r\n");
            }
        }
    }

    fn refresh_screen(&mut self) {
        let mut buf = String::new();

        buf.push_str("\x1b[?25l");
        buf.push_str("\x1b[H");

        self.draw_rows(&mut buf);

        buf.push_str(&format!("\x1b[{};{}H", self.cursor.y, self.cursor.x));

        buf.push_str("\x1b[?25h");

        let _ = stdout_write(buf.as_bytes());
    }

    fn read_key(&mut self) -> Result<Key, char> {
        let c = loop {
            match stdin_read_byte() {
                Ok(b) => break b,
                // technically EAGAIN doesn't happen in
                // raw mode...
                Err(_) if errno() != libc::EAGAIN => die("read"),
                _ => {}
            }
        };
        if c == b'\x1b' {
            let first = stdin_read_byte().map_err(|_| '\x1b')?;
            let second = stdin_read_byte().map_err(|_| '\x1b')?;

            if first == b'[' {
                if second >= b'0' && second <= b'9' {
                    let third = stdin_read_byte().map_err(|_| '\x1b')?;

                    if third == b'~' {
                        match second {
                            b'1' | b'7' => return Ok(Key::Home),
                            b'4' | b'8' => return Ok(Key::End),
                            b'3' => return Ok(Key::Del),
                            b'5' => return Ok(Key::PageUp),
                            b'6' => return Ok(Key::PageDown),
                            _ => {}
                        }
                    }
                }

                match second {
                    b'A' => return Ok(Key::ArrowUp),
                    b'B' => return Ok(Key::ArrowDown),
                    b'C' => return Ok(Key::ArrowRight),
                    b'D' => return Ok(Key::ArrowLeft),
                    b'H' => return Ok(Key::Home),
                    b'F' => return Ok(Key::End),
                    _ => {}
                }
            } else if first == b'O' {
                match second {
                    b'H' => return Ok(Key::Home),
                    b'F' => return Ok(Key::End),
                    _ => {}
                }
            }
            return Err('\x1b');
        }
        Err(c as char)
    }

    fn process_keypress(&mut self) {
        let k = self.read_key();

        match k {
            Err(c) if c as c_char == ctrl_key('q') => {
                let _ = stdout_write(b"\x1b[2J");
                let _ = stdout_write(b"\x1b[H");
                std::process::exit(0);
            }
            Ok(Key::Home) => {
                self.cursor.x = 0;
            }
            Ok(Key::End) => {
                self.cursor.x = self.screen.cols - 1;
            }
            Ok(Key::PageUp) | Ok(Key::PageDown) => {
                let dir = if k == Ok(Key::PageUp) {
                    Key::ArrowUp
                } else {
                    Key::ArrowDown
                };
                for _ in 0..self.screen.rows {
                    self.move_cursor(dir);
                }
            }
            Ok(Key::ArrowLeft) | Ok(Key::ArrowUp) | Ok(Key::ArrowDown) | Ok(Key::ArrowRight) => {
                self.move_cursor(k.unwrap())
            }
            _ => {}
        }
    }

    fn move_cursor(&mut self, key: Key) {
        match key {
            Key::ArrowLeft => {
                if self.cursor.x != 0 {
                    self.cursor.x -= 1;
                }
            }
            Key::ArrowDown => {
                if self.cursor.y != self.screen.rows - 1 {
                    self.cursor.y += 1;
                }
            }
            Key::ArrowUp => {
                if self.cursor.y != 0 {
                    self.cursor.y -= 1;
                }
            }
            Key::ArrowRight => {
                if self.cursor.x != self.screen.cols - 1 {
                    self.cursor.x += 1;
                }
            }
            _ => {}
        }
    }
}

fn main() {
    enable_raw_mode();
    let mut editor = Editor::new();

    loop {
        editor.refresh_screen();
        editor.process_keypress();
    }
}
