use libc::{STDIN_FILENO, STDOUT_FILENO};
use std::ffi::CString;
use std::io::{self, Read, Write};
use std::mem;
use std::os::raw::c_char;
use std::time::{SystemTime, UNIX_EPOCH};

/*** "defines" ***/

const VERSION: &str = "0.0.1";
const TAB_STOP: usize = 8;
const MAX_QUIT_TIMES: usize = 3;

fn ctrl_key(k: char) -> c_char {
    k as i8 & 0x1f
}

#[derive(Clone, Copy, PartialEq)]
enum Key {
    Backspace = 127,
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

use std::convert::TryFrom;

impl TryFrom<u8> for Key {
    type Error = ();

    fn try_from(v: u8) -> Result<Self, Self::Error> {
        match v {
            x if x == Key::Backspace as u8 => Ok(Key::Backspace),
            _ => Err(()),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
enum Highlight {
    Normal = 0,
    Comment,
    Mlcomment,
    Keyword1,
    Keyword2,
    String,
    Number,
    Match,
}

#[derive(Debug, Default, Copy, Clone)]
struct Syntax {
    filetype: &'static str,
    filematch: &'static [&'static str],
    keywords: &'static [&'static str],
    singleline_comment_start: &'static str,
    multiline_comment_start: &'static str,
    multiline_comment_end: &'static str,
    flags: usize,
}

const C_HL_EXTENSIONS: &[&str] = &["c", "h", "cpp"];
const C_HL_KEYWORDS: &[&str] = &[
    "switch",
    "if",
    "while",
    "for",
    "break",
    "continue",
    "return",
    "else",
    "struct",
    "union",
    "typedef",
    "static",
    "enum",
    "class",
    "case",
    "int|",
    "long|",
    "double|",
    "float|",
    "char|",
    "unsigned|",
    "signed|",
    "void|",
];
const HIGHLIGHT_NUMBERS: usize = 1 << 0;
const HIGHLIGHT_STRINGS: usize = 1 << 1;

const HLDB: &[Syntax] = &[Syntax {
    filetype: "c",
    filematch: C_HL_EXTENSIONS,
    keywords: C_HL_KEYWORDS,
    singleline_comment_start: "//",
    multiline_comment_start: "/*",
    multiline_comment_end: "*/",
    flags: HIGHLIGHT_NUMBERS | HIGHLIGHT_STRINGS,
}];

fn is_separator(key: char) -> bool {
    " ,.()+-/*=~%<>[];".contains(key)
}

fn syntax_to_color(hl: Highlight) -> isize {
    use Highlight::*;
    match hl {
        Comment | Mlcomment => 36,
        Keyword1 => 33,
        Keyword2 => 32,
        String => 35,
        Number => 31,
        Match => 34,
        Normal => 37,
    }
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
static mut QUIT_TIMES: usize = MAX_QUIT_TIMES;

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

#[derive(Debug, Default, Clone, Copy)]
struct Cursor {
    x: usize,
    y: usize,
}

#[derive(Debug, Default, Clone, Copy)]
struct Screen {
    rows: usize,
    cols: usize,
}

#[derive(Debug, Default, Clone, Copy)]
struct Offset {
    row: usize,
    col: usize,
}

#[derive(Debug, Clone)]
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

#[derive(Debug, Default, Clone)]
struct Editor {
    cursor: Cursor,
    // cursor.x  => index into row[at].chars
    // editor.rx => index into row[at].render
    render_x: usize,
    screen: Screen,
    offset: Offset,
    rows: Vec<Row>,
    dirty: usize,
    file_name: Option<String>,
    status_msg: StatusMsg,
    syntax: Option<Syntax>,

    // static local in C
    last_match: i32,
    direction: i32,
    saved_hl_line: usize,
    saved_hl: Vec<Highlight>,
}

#[derive(Debug, Clone)]
struct Row {
    chars: String,
    render: String,
    hl: Vec<Highlight>,
    hl_open_comment: bool,
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

        let mut editor = Self {
            cursor,
            screen,
            file_name: filename,
            rows: vec![],
            last_match: -1,
            direction: 1,
            ..Self::default()
        };

        editor.select_syntax_highlight();

        editor.rows = match &editor.file_name {
            Some(f) => read_lines(f)
                .unwrap_or_else(|_| die("fopen"))
                .map(Result::unwrap)
                .map(|chars| {
                    let mut row = Row {
                        render: "".into(),
                        chars,
                        hl: vec![],
                        hl_open_comment: false,
                    };
                    row.update_render();
                    row
                })
                .collect(),
            None => vec![],
        };

        for y in 0..editor.rows.len() {
            editor.update_syntax(y);
        }

        editor
    }

    fn insert_char(&mut self, c: char) {
        if self.cursor.y == self.rows.len() {
            self.rows.push(Row {
                chars: "".into(),
                render: "".into(),
                hl: vec![],
                hl_open_comment: false,
            });
        }
        let row = &mut self.rows[self.cursor.y];
        row.chars.insert(self.cursor.x, c);
        self.update_row(self.cursor.y);
        self.cursor.x += 1;
        self.dirty += 1;
    }

    fn insert_new_line(&mut self) {
        if self.cursor.x == 0 {
            self.rows.insert(
                self.cursor.y,
                Row {
                    chars: "".into(),
                    render: "".into(),
                    hl: vec![],
                    hl_open_comment: false,
                },
            );
        } else {
            let curr_row = &mut self.rows[self.cursor.y];
            let half_right = curr_row.chars.chars().skip(self.cursor.x).collect();
            // keep half left
            curr_row.chars.truncate(self.cursor.x);
            self.update_row(self.cursor.y);
            self.rows.insert(
                self.cursor.y + 1,
                Row {
                    chars: half_right,
                    render: "".into(),
                    hl: vec![],
                    hl_open_comment: false,
                },
            );
            self.update_row(self.cursor.y + 1);
        }
        self.cursor.y += 1;
        self.cursor.x = 0;
        self.dirty += 1;
    }

    fn delete_char(&mut self) {
        if self.cursor.y == self.rows.len() {
            return;
        }
        if self.cursor.x == 0 && self.cursor.y == 0 {
            return;
        }
        if self.cursor.x > 0 {
            let row = &mut self.rows[self.cursor.y];
            if self.cursor.x < row.chars.len() {
                row.chars.remove(self.cursor.x - 1);
                self.update_row(self.cursor.y);
                self.dirty += 1;
            }
            self.cursor.x -= 1;
        } else {
            self.cursor.x = self.rows[self.cursor.y - 1].chars.len();
            let Row { chars, .. } = self.rows.remove(self.cursor.y);
            let prev_row = &mut self.rows[self.cursor.y - 1];
            prev_row.chars.push_str(&chars);
            self.update_row(self.cursor.y - 1);
            self.rows.remove(self.cursor.y);
            self.cursor.y -= 1;
        }
    }

    fn update_row(&mut self, y: usize) {
        self.rows[y].update_render();
        self.update_syntax(y);
    }

    fn prompt(
        &mut self,
        prompt: &str,
        callback: Option<impl Fn(&mut Self, &str, Result<Key, char>)>,
    ) -> Option<String> {
        let prompt = prompt.to_string();
        let mut buf = String::with_capacity(128);

        loop {
            self.set_status_message(prompt.replace("{}", &buf));
            self.refresh_screen();

            let k = self.read_key();
            if k == Ok(Key::Del) || k == Err(ctrl_key('h') as u8 as char) || k == Ok(Key::Backspace)
            {
                buf.pop();
            } else if k == Err(b'\x1b' as char) {
                self.set_status_message("".into());
                if let Some(cb) = callback.as_ref() {
                    cb(self, &buf, k);
                }
                return None;
            } else if k == Err('\r') {
                if !buf.is_empty() {
                    self.set_status_message("".into());
                    if let Some(cb) = callback.as_ref() {
                        cb(self, &buf, k);
                    }
                    return Some(buf);
                }
            } else if let Err(c) = k {
                if unsafe { libc::iscntrl(c as i32) } == 0 && (c as u32) < 128 {
                    buf.push(c);
                }
            }

            if let Some(cb) = callback.as_ref() {
                cb(self, &buf, k);
            }
        }
    }

    fn save(&mut self) {
        if self.file_name.is_none() {
            self.file_name = self.prompt(
                "Save as: {}",
                None::<fn(&mut Self, &str, Result<Key, char>)>,
            );
            if self.file_name.is_none() {
                self.set_status_message("Save aborted".into());
                return;
            }
            self.select_syntax_highlight();
        }
        let file_name = self.file_name.as_ref().unwrap();

        let whole_file = self
            .rows
            .iter()
            .map(|row| row.chars.as_str())
            .collect::<Vec<_>>()
            .join("\n");
        let bytes = whole_file.bytes().len();

        if let Err(err) = std::fs::write(file_name, whole_file) {
            self.set_status_message(format!("Can't save! I/O error: {err}"));
        } else {
            self.set_status_message(format!("{} bytes written to disk", bytes));
            self.dirty = 0;
        }
    }

    fn find_callback(&mut self, query: &str, key: Result<Key, char>) {
        if !self.saved_hl.is_empty() {
            self.rows[self.saved_hl_line].hl = self.saved_hl.clone();
            self.saved_hl.clear();
        }

        if key == Err('\r') || key == Err('\x1b') {
            self.last_match = -1;
            self.direction = 1;
            return;
        } else if key == Ok(Key::ArrowRight) || key == Ok(Key::ArrowDown) {
            self.direction = 1;
        } else if key == Ok(Key::ArrowLeft) || key == Ok(Key::ArrowUp) {
            self.direction = -1;
        } else {
            self.last_match = -1;
            self.direction = 1;
        }

        if self.last_match == -1 {
            self.direction = 1;
        }
        let mut current = self.last_match;
        for _ in 0..self.rows.len() {
            current += self.direction;
            if current == -1 {
                current = self.rows.len() as i32 - 1;
            } else if current == self.rows.len() as i32 {
                current = 0
            }

            if let Some(match_) = self.rows[current as usize].render.find(query) {
                self.last_match = current;
                self.cursor.y = current as usize;
                self.cursor.x = self.row_rx_to_cx(&self.rows[current as usize], match_);
                self.offset.row = self.rows.len();

                self.saved_hl_line = current as usize;
                self.saved_hl = self.rows[current as usize].hl.clone();
                for i in match_..query.len() {
                    self.rows[current as usize].hl[i] = Highlight::Match;
                }
                break;
            }
        }
    }

    fn find(&mut self) {
        let saved_cursor = self.cursor;
        let saved_offset = self.offset;

        if let None = self.prompt(
            "Search: %s (Use ESC/Arrows/Enter)",
            Some(Self::find_callback),
        ) {
            self.cursor = saved_cursor;
            self.offset = saved_offset;
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

    fn row_rx_to_cx(&self, row: &Row, rx: usize) -> usize {
        let mut curr_rx = 0;
        let mut cx = 0;
        for ch in row.chars.chars() {
            if ch == '\t' {
                curr_rx += (TAB_STOP - 1) - (curr_rx % TAB_STOP);
            }
            curr_rx += 1;
            cx += 1;
            if curr_rx > rx {
                return cx;
            }
        }
        cx
    }

    fn scroll(&mut self) {
        self.render_x = 0;

        if self.cursor.y < self.rows.len() {
            self.render_x = self.row_cx_to_rx();
        }

        if self.cursor.y < self.offset.row {
            self.offset.row = self.cursor.y;
        }
        if self.cursor.y >= self.offset.row + self.screen.rows {
            self.offset.row = self.cursor.y - self.screen.rows + 1;
        }

        if self.render_x < self.offset.col {
            self.offset.col = self.render_x;
        }
        if self.render_x >= self.offset.col + self.screen.cols {
            self.offset.col = self.render_x - self.screen.cols + 1;
        }
    }

    fn set_status_message(&mut self, msg: String) {
        self.status_msg.msg = msg;
        self.status_msg.time = SystemTime::now();
    }

    fn draw_rows(&self, buf: &mut String) {
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
                let hl = &self.rows[file_row].hl[self.offset.col..];
                let mut curr_color = -1;
                for i in 0..len as usize {
                    if let Some(ch) = self.rows[file_row]
                        .render
                        .bytes()
                        .skip(self.offset.col)
                        .nth(i)
                    {
                        if unsafe { libc::iscntrl(ch as i32) } != 0 {
                            let sym = if ch <= 26 { b'@' + ch } else { b'?' };
                            buf.push_str("\x1b[7m");
                            // prob will explode, need to change String -> Vec<u8>
                            buf.push(sym as char);
                            buf.push_str("\x1b[m");
                            if curr_color != -1 {
                                buf.push_str(&format!("\x1b[{curr_color}m"));
                            }
                        } else if hl[i] == Highlight::Normal {
                            // kaboom
                            if curr_color != -1 {
                                buf.push_str("\x1b[39m");
                                curr_color = -1;
                            }
                            buf.push(ch as char);
                        } else {
                            let color = syntax_to_color(hl[i]);
                            if color != curr_color {
                                curr_color = color;
                                buf.push_str(&format!("\x1b[{color}m"));
                            }
                            buf.push(ch as char);
                        }
                    }
                }
            }

            buf.push_str("\x1b[K");
            buf.push_str("\r\n");
        }
    }

    fn draw_status_bar(&mut self, buf: &mut String) {
        buf.push_str("\x1b[7m");

        let filename = self.file_name.as_deref().unwrap_or("[No Name]");
        let dirty = if self.dirty > 0 { "(modified)" } else { "" };
        let mut status = format!("{filename:.20} - {} lines {}", self.rows.len(), dirty);

        let mut len = status.len();
        if len > self.screen.cols {
            len = self.screen.cols;
            status.truncate(len);
        }
        buf.push_str(&status);

        let filetype = if let Some(syntax) = self.syntax {
            syntax.filetype
        } else {
            "no ft"
        };
        let row_status = format!("{} | {}/{}", filetype, self.cursor.y + 1, self.rows.len());

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

        let msg_len = std::cmp::min(self.status_msg.msg.len(), self.screen.cols);
        let elapsed = self.status_msg.time.elapsed().unwrap().as_secs();

        if elapsed < 5 {
            for c in self.status_msg.msg.bytes().take(msg_len) {
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
            self.render_x - self.offset.col + 1
        ));

        buf.push_str("\x1b[?25h");

        let _ = stdout_write(buf.as_bytes());
    }

    fn read_key(&mut self) -> Result<Key, char> {
        let mut n_read = 0;
        let mut c: c_char = 0;
        while n_read != 1 {
            // left libc::read because there's no easy way to catch
            // EAGAIN in Rust's std (there's a way, but it's convoluted)
            n_read = unsafe { libc::read(STDIN_FILENO, mem::transmute(&mut c), 1) } as i32;
            if n_read == -1 && errno() != libc::EAGAIN {
                die("read");
            }
        }
        if c as u8 == b'\x1b' {
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
        if let Ok(k) = (c as u8).try_into() {
            return Ok(k);
        }
        Err(c as u8 as char)
    }

    fn process_keypress(&mut self) {
        let k = self.read_key();

        match k {
            Err(c) if c as c_char == ctrl_key('q') => {
                if self.dirty > 0 && unsafe { QUIT_TIMES } > 0 {
                    self.set_status_message(format!(
                        "WARNING!!! File has unsaved changes. Press Ctrl-Q {} more times to quit.",
                        unsafe { QUIT_TIMES }
                    ));
                    unsafe { QUIT_TIMES -= 1 };
                    return;
                }

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
            Ok(Key::Del) => {
                self.move_cursor(Key::ArrowRight);
                self.delete_char();
            }
            Ok(Key::Backspace) => {
                self.delete_char();
            }
            Err(c) if c as c_char == ctrl_key('h') => {
                self.delete_char();
            }
            Err(c) if c as c_char == ctrl_key('s') => {
                self.save();
            }
            Err(c) if c as c_char == ctrl_key('f') => {
                self.find();
            }
            Err(c) if c as c_char == ctrl_key('l') || c as u8 == b'\x1b' => { /* do nothing */ }
            Err(c) if c == '\r' => self.insert_new_line(),
            Err(c) => self.insert_char(c),
        }

        unsafe { QUIT_TIMES = MAX_QUIT_TIMES };
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

    fn select_syntax_highlight(&mut self) {
        self.syntax = None;

        if self.file_name.is_none() {
            return;
        }

        // lol, should hold path mam
        let ext = self
            .file_name
            .as_ref()
            .unwrap()
            .split('.')
            .skip(1)
            .next()
            .unwrap();

        self.syntax = match HLDB.iter().find(|syntax| syntax.filematch.contains(&ext)) {
            None => return,
            Some(syntax) => Some(*syntax),
        };

        for y in 0..self.rows.len() {
            self.update_syntax(y);
        }
    }

    fn update_syntax(&mut self, y: usize) {
        self.rows[y].hl = vec![Highlight::Normal; self.rows[y].render.len()];

        let syntax = match self.syntax {
            None => return,
            Some(syntax) => syntax,
        };

        let keywords = syntax.keywords;

        let scs = syntax.singleline_comment_start;
        let mcs = syntax.multiline_comment_start;
        let mce = syntax.multiline_comment_end;

        let mut prev_sep = true;
        let mut in_string = None;
        let mut in_comment = y > 0 && self.rows[y - 1].hl_open_comment;

        let mut i = 0;
        'outer: while i < self.rows[y].render.len() {
            let c = self.rows[y].render.chars().nth(i).unwrap();
            let prev_hl = if i > 0 {
                self.rows[y].hl[i - 1]
            } else {
                Highlight::Normal
            };
            if scs.len() > 0 && in_string.is_none() && !in_comment {
                let from_i = &self.rows[y].render.as_bytes()[i..];
                if from_i == scs.as_bytes() {
                    for j in i..self.rows[y].render.len() {
                        self.rows[y].hl[j] = Highlight::Comment;
                    }
                    break;
                }
            }

            if mcs.len() > 0 && mce.len() > 0 && in_string.is_none() {
                if in_comment {
                    self.rows[y].hl[i] = Highlight::Mlcomment;
                    let from_i = &self.rows[y].render.as_bytes()[i..];
                    if from_i == mce.as_bytes() {
                        for j in i..mce.len() {
                            self.rows[y].hl[j] = Highlight::Mlcomment;
                        }
                        i += mce.len();
                        in_comment = false;
                        prev_sep = true;
                    } else {
                        i += 1;
                    }
                    continue;
                } else if &self.rows[y].render.as_bytes()[i..] == mcs.as_bytes() {
                    for j in i..mcs.len() {
                        self.rows[y].hl[j] = Highlight::Mlcomment;
                    }
                    i += mcs.len();
                    in_comment = true;
                    continue;
                }
            }

            if syntax.flags & HIGHLIGHT_STRINGS != 0 {
                if let Some(in_str) = in_string {
                    self.rows[y].hl[i] = Highlight::String;
                    if c == '\\' && i + 1 < self.rows[y].render.len() {
                        self.rows[y].hl[i + 1] = Highlight::String;
                        i += 2;
                        continue;
                    }
                    if c == in_str {
                        in_string = None;
                    }
                    i += 1;
                    prev_sep = true;
                    continue;
                } else {
                    if c == '"' || c == '\'' {
                        in_string = Some(c);
                        self.rows[y].hl[i] = Highlight::String;
                        i += 1;
                        continue;
                    }
                }
            }

            if syntax.flags & HIGHLIGHT_NUMBERS != 0 {
                if c.is_ascii_digit() && (prev_sep || prev_hl == Highlight::Number)
                    || (c == '.' && prev_hl == Highlight::Number)
                {
                    self.rows[y].hl[i] = Highlight::Number;
                    i += 1;
                    prev_sep = false;
                    continue;
                }
            }

            if prev_sep {
                for (j, keyword) in keywords.into_iter().enumerate() {
                    let kw2 = keyword.ends_with('|');
                    let k_len = if kw2 {
                        keyword.len() - 1
                    } else {
                        keyword.len()
                    };

                    let max_render_len = std::cmp::min(i + k_len, self.rows[y].render.len());
                    if &self.rows[y].render.as_bytes()[i..max_render_len]
                        == &keyword.as_bytes()[..k_len]
                        && is_separator(self.rows[y].render.as_bytes()[i + k_len] as char)
                    {
                        for k in i..k_len {
                            self.rows[y].hl[k] = if kw2 {
                                Highlight::Keyword2
                            } else {
                                Highlight::Keyword1
                            };
                        }
                        i += k_len;
                        if j != keywords.len() - 1 {
                            prev_sep = false;
                            continue 'outer;
                        }
                        break;
                    }
                }
            }

            prev_sep = is_separator(c);
            i += 1;
        }

        let changed = self.rows[y].hl_open_comment != in_comment;
        self.rows[y].hl_open_comment = in_comment;
        if changed && y + 1 < self.rows.len() {
            self.update_syntax(y + 1);
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

    editor.set_status_message("HELP: Ctrl-S = save | Ctrl-Q = quit | Ctrl-F = find".into());

    loop {
        editor.refresh_screen();
        editor.process_keypress();
    }
}
