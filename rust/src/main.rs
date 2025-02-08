use libc::{STDIN_FILENO, STDOUT_FILENO};
use std::ffi::CString;
use std::io::{self, Read, Write};
use std::mem;
use std::os::raw::c_char;
use std::time::{SystemTime, UNIX_EPOCH};

/*** "defines" ***/

const VERSION: &str = "0.0.1";
const TAB_STOP: usize = 8;

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

#[derive(Debug, Default)]
struct Offset {
    row: usize,
    col: usize,
}

#[derive(Debug)]
struct StatusMsg {
    msg: String,
    time: SystemTime,
}

impl Default for StatusMsg {
    fn default() -> Self {
        StatusMsg {
            msg: String::new(),
            time: UNIX_EPOCH,
        }
    }
}

#[derive(Debug, Default)]
struct Editor {
    cursor: Cursor,
    // cursor.x  => index into row[at].chars
    // editor.rx => index into row[at].render
    rx: usize,
    screen: Screen,
    offset: Offset,
    rows: Vec<Row>,
    filename: Option<String>,
    statusmsg: StatusMsg,
}

#[derive(Debug)]
struct Row {
    chars: String,
    render: String,
}

impl Row {
    // perhaps move this from here later
    fn update_render(&mut self) {
        let tab_count = self.chars.chars().filter(|&c| c == '\t').count();
        let estimated_size = self.chars.len() + tab_count * (TAB_STOP - 1);

        let mut render = String::with_capacity(estimated_size);
        for c in self.chars.chars() {
            if c == '\t' {
                let spaces_to_add = TAB_STOP - (render.len() % TAB_STOP);
                render.extend(std::iter::repeat(' ').take(spaces_to_add));
            } else {
                render.push(c);
            }
        }

        self.render = render;
    }
}

use std::fs::File;
use std::io::BufRead;
use std::path::Path;

fn read_lines<P>(filename: P) -> io::Result<io::Lines<io::BufReader<File>>>
where
    P: AsRef<Path>,
{
    let file = File::open(filename)?;
    Ok(io::BufReader::new(file).lines())
}

impl Editor {
    fn new(filename: Option<String>) -> Self {
        let cursor = Cursor::default();
        let mut screen = get_window_size().unwrap_or_else(|| die("get_window_size"));
        screen.rows -= 2;

        let rows = match &filename {
            Some(f) => read_lines(f)
                .unwrap_or_else(|_| die("fopen"))
                .map(Result::unwrap)
                .map(|chars| {
                    let mut row = Row {
                        render: "".into(),
                        chars,
                    };
                    row.update_render();
                    row
                })
                .collect(),
            None => vec![],
        };

        Self {
            cursor,
            screen,
            filename,
            rows,
            ..Self::default()
        }
    }

    fn row_cx_to_rx(&self) -> usize {
        let mut rx = 0;
        let row = &self.rows[self.cursor.y];
        for i in 0..self.cursor.x {
            if row.chars.bytes().nth(i) == Some(b'\t') {
                rx += (TAB_STOP - 1) - (rx % TAB_STOP);
            }
            rx += 1;
        }
        rx
    }

    fn scroll(&mut self) {
        self.rx = 0;

        if self.cursor.y < self.rows.len() {
            self.rx = self.row_cx_to_rx();
        }

        if self.cursor.y < self.offset.row {
            self.offset.row = self.cursor.y;
        }
        if self.cursor.y >= self.offset.row + self.screen.rows {
            self.offset.row = self.cursor.y - self.screen.rows + 1;
        }

        if self.rx < self.offset.col {
            self.offset.col = self.rx;
        }
        if self.rx >= self.offset.col + self.screen.cols {
            self.offset.col = self.rx - self.screen.cols + 1;
        }
    }

    fn set_status_message(&mut self, msg: String) {
        self.statusmsg.msg = msg;
        self.statusmsg.time = SystemTime::now();
    }

    fn draw_rows(&mut self, buf: &mut String) {
        for y in 0..self.screen.rows {
            let file_row = y + self.offset.row;
            if file_row >= self.rows.len() {
                if self.rows.len() == 0 && y == self.screen.rows / 3 {
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
            } else {
                let mut len = self.rows[file_row].render.len() as isize - self.offset.col as isize;
                if len < 0 {
                    len = 0;
                }
                if len > self.screen.cols as isize {
                    len = self.screen.cols as isize;
                }
                for i in 0..len as usize {
                    if let Some(ch) = self.rows[file_row].render.bytes().nth(i) {
                        buf.push(ch as char);
                    }
                }
            }

            buf.push_str("\x1b[K");
            buf.push_str("\r\n");
        }
    }

    fn draw_status_bar(&mut self, buf: &mut String) {
        buf.push_str("\x1b[7m");

        let filename = self.filename.as_deref().unwrap_or("[No Name]");
        let mut status = format!("{filename:.20} - {} lines", self.rows.len());

        let mut len = status.len();
        if len > self.screen.cols {
            len = self.screen.cols;
            status.truncate(len);
        }
        buf.push_str(&status);

        let row_status = format!("{}/{}", self.cursor.y + 1, self.rows.len());

        while len < self.screen.cols {
            if self.screen.cols - len == row_status.len() {
                buf.push_str(&row_status);
                break;
            }
            buf.push(' ');
            len += 1;
        }

        buf.push_str("\x1b[m");
        buf.push_str("\r\n");
    }

    fn draw_message_bar(&self, buf: &mut String) {
        buf.push_str("\x1b[K");

        let msg_len = std::cmp::min(self.statusmsg.msg.len(), self.screen.cols);
        let elapsed = self.statusmsg.time.elapsed().unwrap().as_secs();

        if elapsed < 5 {
            for c in self.statusmsg.msg.bytes().take(msg_len) {
                buf.push(c as char);
            }
        }
    }

    fn refresh_screen(&mut self) {
        self.scroll();

        let mut buf = String::new();

        buf.push_str("\x1b[?25l");
        buf.push_str("\x1b[H");

        self.draw_rows(&mut buf);
        self.draw_status_bar(&mut buf);
        self.draw_message_bar(&mut buf);

        buf.push_str(&format!(
            "\x1b[{};{}H",
            self.cursor.y - self.offset.row + 1,
            self.rx - self.offset.col + 1
        ));

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
                if self.cursor.y < self.rows.len() {
                    self.cursor.x = self.rows[self.cursor.y].chars.len();
                }
            }
            Ok(Key::PageUp) | Ok(Key::PageDown) => {
                let dir = if k == Ok(Key::PageUp) {
                    self.cursor.y = self.offset.row;
                    Key::ArrowUp
                } else {
                    self.cursor.y = self.offset.row + self.screen.rows - 1;
                    self.cursor.y = std::cmp::min(self.cursor.y, self.rows.len());
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
        let row = if self.cursor.y >= self.rows.len() {
            None
        } else {
            Some(&self.rows[self.cursor.y])
        };

        match key {
            Key::ArrowLeft => {
                if self.cursor.x != 0 {
                    self.cursor.x -= 1;
                } else if self.cursor.y > 0 {
                    self.cursor.y -= 1;
                    self.cursor.x = self.rows[self.cursor.y].chars.len();
                }
            }
            Key::ArrowDown => {
                if self.cursor.y < self.rows.len() {
                    self.cursor.y += 1;
                }
            }
            Key::ArrowUp => {
                if self.cursor.y != 0 {
                    self.cursor.y -= 1;
                }
            }
            Key::ArrowRight => {
                if let Some(row) = row {
                    if self.cursor.x < row.chars.len() {
                        self.cursor.x += 1;
                    } else if self.cursor.x == row.chars.len() {
                        self.cursor.y += 1;
                        self.cursor.x = 0;
                    }
                }
            }
            _ => {}
        }

        let row = if self.cursor.y >= self.rows.len() {
            None
        } else {
            Some(&self.rows[self.cursor.y])
        };
        let row_len = row.map(|r| r.chars.len()).unwrap_or_default();
        if self.cursor.x > row_len {
            self.cursor.x = row_len;
        }
    }
}

fn main() {
    enable_raw_mode();
    let args: Vec<_> = std::env::args().skip(1).collect();
    let filename = if args.len() >= 1 {
        Some(args[0].clone())
    } else {
        None
    };
    let mut editor = Editor::new(filename);

    editor.set_status_message("HELP: Ctrl-Q = quit".into());

    loop {
        editor.refresh_screen();
        editor.process_keypress();
    }
}
