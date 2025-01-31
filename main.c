/*** imports ***/

#define _DEFAULT_SOURCE
#define _BSD_SOURCE
#define _GNU_SOURCE

#include <ctype.h>
#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include <sys/ioctl.h>
#include <sys/types.h>
#include <termios.h>
#include <time.h>
#include <unistd.h>

/*** defines ***/

#define VERSION "0.0.1"
#define KILO_TAB_STOP 8
#define KILO_QUIT_TIMES 3

#define CTRL_KEY(k) ((k) & 0x1f)

enum EditorKey {
    BACKSPACE = 127,
    ARROW_LEFT = 1000,
    ARROW_RIGHT,
    ARROW_UP,
    ARROW_DOWN,
    DEL_KEY,
    HOME_KEY,
    END_KEY,
    PAGE_UP,
    PAGE_DOWN,
};

enum EditorHighlight {
    HL_NORMAL = 0,
    HL_NUMBER
};

/*** data ***/

typedef struct Row {
    int size;
    int rsize;
    char *chars;
    char *render;
    unsigned char *hl;
} Row;

struct EditorConfig {
    int cx, cy;
    int rx;
    int rowoff;
    int coloff;
    int screenrows;
    int screencols;
    int numrows;
    Row *row;
    int dirty;
    char *filename;
    char statusmsg[80];
    time_t statusmsg_time;
    struct termios orig_termios;
};

struct EditorConfig E;

/*** prototypes ***/

void editor_set_status_message(const char *fmt, ...);
void editor_refresh_screen(void);
char *editor_prompt(char *prompt, void (*callback) (char*, int));

/*** terminal ***/

void die(const char *s) {
    write(STDOUT_FILENO, "\x1b[2J", 4);
    write(STDOUT_FILENO, "\x1b[H", 3);

    perror(s);
    exit(1);
}

void disable_raw_mode(void) {
    if (tcsetattr(STDIN_FILENO, TCSAFLUSH, &E.orig_termios) == -1)
        die("tcsetattr");
}

void enable_raw_mode(void) {
    if (tcgetattr(STDIN_FILENO, &E.orig_termios) == -1)
        die("tcgetattr");

    atexit(disable_raw_mode);

    struct termios raw = E.orig_termios;

    raw.c_iflag &= ~(BRKINT | ICRNL | INPCK | ISTRIP | IXON);
    raw.c_oflag &= ~(OPOST);
    raw.c_cflag |= (CS8);
    raw.c_lflag &= ~(ECHO | ICANON | IEXTEN | ISIG);
    raw.c_cc[VMIN] = 0;
    raw.c_cc[VTIME] = 1;

    if (tcsetattr(STDIN_FILENO, TCSAFLUSH, &raw) == -1)
        die("tcsetattr");
}

int editor_read_key(void) {
    int nread;
    char c;
    while ((nread = read(STDIN_FILENO, &c, 1)) != 1) {
        if (nread == -1 && errno != EAGAIN)
            die("read");
    }
    if (c == '\x1b') {
        char seq[3];
        if (read(STDIN_FILENO, &seq[0], 1) != 1) return '\x1b';
        if (read(STDIN_FILENO, &seq[1], 1) != 1) return '\x1b';

        if (seq[0] == '[') {
            if (seq[1] >= '0' && seq[1] <= '9') {
                if (read(STDIN_FILENO, &seq[2], 1) != 1) return '\x1b';
                if (seq[2] == '~') {
                    switch (seq[1]) {
                        case '1': return HOME_KEY;
                        case '3': return DEL_KEY;
                        case '4': return END_KEY;
                        case '5': return PAGE_UP;
                        case '6': return PAGE_DOWN;
                        case '7': return HOME_KEY;
                        case '8': return END_KEY;
                    }
                }
            } else {
                switch (seq[1]) {
                    case 'A': return ARROW_UP;
                    case 'B': return ARROW_DOWN;
                    case 'C': return ARROW_RIGHT;
                    case 'D': return ARROW_LEFT;
                    case 'H': return HOME_KEY;
                    case 'F': return END_KEY;
                }
            }
        } else if (seq[0] == 'O') {
            switch (seq[1]) {
                case 'H': return HOME_KEY;
                case 'F': return END_KEY;
            }
        }
        return '\x1b';
    }
    return c;
}

int get_cursor_position(int *rows, int *cols) {
    char buf[32];
    unsigned int i = 0;

    if (write(STDOUT_FILENO, "\x1b[6n", 4) != 4) return -1;

    while (i < sizeof(buf) - 1) {
        if (read(STDIN_FILENO, &buf[i], 1) != 1) break;
        if (buf[i] == 'R') break;
        i++;
    }
    buf[i] = '\0';

    if (buf[0] != '\x1b' || buf[1] != '[') return -1;
    if (sscanf(&buf[2], "%d;%d", rows, cols) != 2) return -1;

    return 0;
}

int get_window_size(int *rows, int *cols) {
    struct winsize ws;

    if (ioctl(STDOUT_FILENO, TIOCGWINSZ, &ws) == -1 || ws.ws_col == 0) {
        if (write(STDOUT_FILENO, "\x1b[999C\x1b[999B", 12) != 12) return -1;
        return get_cursor_position(rows, cols);
    }

    *cols = ws.ws_col;
    *rows = ws.ws_row;
    return 0;
}

/*** syntax highlighting ***/

void editor_update_syntax(Row *row) {
    row->hl = realloc(row->hl, row->rsize);
    memset(row->hl, HL_NORMAL, row->rsize);

    for (int i = 0; i < row->rsize; i++) {
        if (isdigit(row->render[i])) {
            row->hl[i] = HL_NUMBER;
        }
    }
}

int editor_syntax_to_color(int hl) {
    switch (hl) {
        case HL_NUMBER: return 31;
        default: return 37;
    }
}

/*** row operations ***/

int editor_row_cx_to_rx(Row *row, int cx) {
    int rx = 0;
    for (int j = 0; j < cx; j++) {
        if (row->chars[j] == '\t')
            rx += (KILO_TAB_STOP - 1) - (rx % KILO_TAB_STOP);
        rx++;
    }
    return rx;
}

int editor_row_rx_to_cx(Row *row, int rx) {
    int curr_rx = 0;
    int cx;
    for (cx = 0; cx < row->size; cx++) {
        if (row->chars[cx] == '\t')
            curr_rx += (KILO_TAB_STOP - 1) - (curr_rx % KILO_TAB_STOP);
        curr_rx++;

        if (curr_rx > rx) return cx;
    }
    return cx;
}

void editor_update_row(Row *row) {
    int tabs = 0;

    for (int j = 0; j < row->size; j++) {
        if (row->chars[j] == '\t') tabs++;
    }

    free(row->render);
    row->render = malloc(row->size + tabs*(KILO_TAB_STOP - 1) + 1);

    int idx = 0;
    for (int j = 0; j < row->size; j++) {
        if (row->chars[j] == '\t') {
            do {
                row->render[idx++] = ' ';
            } while (idx % KILO_TAB_STOP != 0);
        } else {
            row->render[idx++] = row->chars[j];
        }
    }
    row->render[idx] = '\0';
    row->rsize = idx;

    editor_update_syntax(row);
}

void editor_insert_row(int at, char *s, size_t len) {
    if (at < 0 || at > E.numrows) return;

    E.row = realloc(E.row, sizeof(Row) * (E.numrows + 1));
    memmove(&E.row[at + 1], &E.row[at], sizeof(Row) * (E.numrows - at));

    E.row[at].size = len;
    E.row[at].chars = malloc(len + 1);
    memcpy(E.row[at].chars, s, len);
    E.row[at].chars[len] = '\0';

    E.row[at].rsize = 0;
    E.row[at].render = NULL;
    E.row[at].hl = NULL;
    editor_update_row(&E.row[at]);

    E.numrows++;
    E.dirty++;
}

void editor_free_row(Row *row) {
    free(row->render);
    free(row->chars);
    free(row->hl);
}

void editor_del_row(int at) {
    if (at < 0 || at >= E.numrows) return;
    editor_free_row(&E.row[at]);
    memmove(&E.row[at], &E.row[at + 1], sizeof(Row) * (E.numrows - at - 1));
    E.numrows--;
    E.dirty++;
}

void editor_row_insert_char(Row *row, int at, int c) {
    if (at < 0 || at > row->size) at = row->size;
    row->chars = realloc(row->chars, row->size + 2);
    memmove(&row->chars[at + 1], &row->chars[at], row->size - at + 1);
    row->size++;
    row->chars[at] = c;
    editor_update_row(row);
    E.dirty++;
}

void editor_row_append_string(Row *row, char *s, size_t len) {
    row->chars = realloc(row->chars, row->size + len + 1);
    memcpy(&row->chars[row->size], s, len);
    row->size += len;
    row->chars[row->size] = '\0';
    editor_update_row(row);
    E.dirty++;
}

void editor_row_del_char(Row *row, int at) {
    if (at < 0 || at >= row->size) return;
    memmove(&row->chars[at], &row->chars[at + 1], row->size - at);
    row->size--;
    editor_update_row(row);
    E.dirty++;
}

/*** editor operations ***/

void editor_insert_char(int c) {
    if (E.cy == E.numrows)
        editor_insert_row(E.numrows, "", 0);
    editor_row_insert_char(&E.row[E.cy], E.cx, c);
    E.cx++;
}

void editor_insert_new_line(void) {
    if (E.cx == 0) {
        editor_insert_row(E.cy, "", 0);
    } else {
        Row *row = &E.row[E.cy];
        editor_insert_row(E.cy + 1, &row->chars[E.cx], row->size - E.cx);
        row = &E.row[E.cy];
        row->size = E.cx;
        row->chars[row->size] = '\0';
        editor_update_row(row);
    }
    E.cy++;
    E.cx = 0;
}

void editor_del_char(void) {
    if (E.cy == E.numrows) return;
    if (E.cx == 0 && E.cy == 0) return;

    Row *row = &E.row[E.cy];
    if (E.cx > 0) {
        editor_row_del_char(row, E.cx - 1);
        E.cx--;
    } else {
        E.cx = E.row[E.cy - 1].size;
        editor_row_append_string(&E.row[E.cy - 1], row->chars, row->size);
        editor_del_row(E.cy);
        E.cy--;
    }
}

/*** file i/o ***/

char *editor_rows_to_string(int *buf_len) {
    int total_len = 0;
    for (int j = 0; j < E.numrows; j++)
        total_len += E.row[j].size + 1;
    *buf_len = total_len;

    char *buf = malloc(total_len);
    char *p = buf;
    for (int j = 0; j < E.numrows; j++) {
        memcpy(p, E.row[j].chars, E.row[j].size);
        p += E.row[j].size;
        *p = '\n';
        p++;
    }

    return buf;
}

void editor_open(char *filename) {
    free(E.filename);
    E.filename = strdup(filename);

    FILE *fp = fopen(filename, "r");
    if (!fp) die("fopen");

    char *line = NULL;
    size_t line_cap = 0;
    ssize_t line_len;

    while ((line_len = getline(&line, &line_cap, fp)) != -1) {
        while (line_len > 0 && (line[line_len - 1] == '\n' ||
                                line[line_len - 1] == '\r'))
            line_len--;
        editor_insert_row(E.numrows, line, line_len);
    }

    free(line);
    fclose(fp);
    E.dirty = 0;
}

void editor_save(void) {
    if (E.filename == NULL) {
        E.filename = editor_prompt("Save as: %s", NULL);
        if (E.filename == NULL) {
            editor_set_status_message("Save aborted");
            return;
        }
    }

    int len;
    char *buf = editor_rows_to_string(&len);

    int fd = open(E.filename, O_RDWR | O_CREAT, 0644);
    if (fd != -1) {
        if (ftruncate(fd, len) != -1) {
            if (write(fd, buf, len) == len) {
                close(fd);
                free(buf);
                E.dirty = 0;
                editor_set_status_message("%d bytes written to disk", len);
                return;
            }
        }
        close(fd);
    }
    free(buf);
    editor_set_status_message("Can't save! I/O error: %s", strerror(errno));
}

/*** find ***/

void editor_find_callback(char *query, int key) {
    static int last_match = -1;
    static int direction = 1;

    if (key == '\r' || key == '\x1b') {
        last_match = -1;
        direction = 1;
        return;
    } else if (key == ARROW_RIGHT || key == ARROW_DOWN) {
        direction = 1;
    } else if (key == ARROW_LEFT || key == ARROW_UP) {
        direction = -1;
    } else {
        last_match = -1;
        direction = 1;
    }

    if (last_match == -1) direction = 1;
    int current = last_match;
    for (int i = 0; i < E.numrows; i++) {
        current += direction;
        if (current == -1) current = E.numrows - 1;
        else if (current == E.numrows) current = 0;

        Row *row = &E.row[current];
        char *match = strstr(row->render, query);
        if (match) {
            last_match = current;
            E.cy = current;
            E.cx = editor_row_rx_to_cx(row, match - row->render);
            E.rowoff = E.numrows;
            break;
        }
    }
}

void editor_find(void) {
    int saved_cx = E.cx;
    int saved_cy = E.cy;
    int saved_coloff = E.coloff;
    int saved_rowoff = E.rowoff;

    char *query = editor_prompt("Search: %s (Use ESC/Arrows/Enter)", editor_find_callback);

    if (query) {
        free(query);
    } else {
        E.cx = saved_cx;
        E.cy = saved_cy;
        E.coloff = saved_coloff;
        E.rowoff = saved_rowoff;
    }
}

/*** append buffer ***/

struct AppendBuf {
    char *data;
    int len;
};

#define ABUF_INIT {NULL, 0}

void abuf_append(struct AppendBuf *abuf, const char *s, int len) {
    char *new = realloc(abuf->data, abuf->len + len);
    if (!new) return;

    memcpy(&new[abuf->len], s, len);
    abuf->data = new;
    abuf->len += len;
}

void abuf_free(struct AppendBuf *abuf) {
    free(abuf->data);
}

/*** output ***/

void editor_scroll(void) {
    E.rx = 0;
    if (E.cy < E.numrows)
        E.rx = editor_row_cx_to_rx(&E.row[E.cy], E.cx);

    if (E.cy < E.rowoff)
        E.rowoff = E.cy;
    if (E.cy >= E.rowoff + E.screenrows)
        E.rowoff = E.cy - E.screenrows + 1;

    if (E.rx < E.coloff)
        E.coloff = E.rx;
    if (E.rx >= E.coloff + E.screencols)
        E.coloff = E.rx - E.screencols + 1;
}

void editor_draw_rows(struct AppendBuf *abuf) {
    for (int y = 0; y < E.screenrows; y++) {
        int filerow = y + E.rowoff;
        if (filerow >= E.numrows) {
            if (E.numrows == 0 && y == E.screenrows / 3) {
                char welcome[80];
                int welcome_len = snprintf(
                    welcome,
                    sizeof(welcome),
                    "Kilo editor -- version %s",
                    VERSION
                );
                if (welcome_len > E.screencols)
                    welcome_len = E.screencols;
                int padding = (E.screencols - welcome_len) / 2;
                if (padding) {
                    abuf_append(abuf, "~", 1);
                    padding--;
                }
                while (padding--)
                    abuf_append(abuf, " ", 1);

                abuf_append(abuf, welcome, welcome_len);
            } else {
                abuf_append(abuf, "~", 1);
            }
        } else {
            int len = E.row[filerow].rsize - E.coloff;
            if (len < 0) len = 0;
            if (len > E.screencols) len = E.screencols;
            char *c = &E.row[filerow].render[E.coloff];
            for (int j = 0; j < len; j++) {
                if (isdigit(c[j])) {
                    abuf_append(abuf, "\x1b[31m", 5);
                    abuf_append(abuf, &c[j], 1);
                    abuf_append(abuf, "\x1b[39m", 5);
                } else {
                    abuf_append(abuf, &c[j], 1);
                }
            }
        }

        abuf_append(abuf, "\x1b[K", 3);
        abuf_append(abuf, "\r\n", 2);
    }
}

void editor_draw_status_bar(struct AppendBuf *abuf) {
    abuf_append(abuf, "\x1b[7m", 4);

    char status[80], rstatus[80];
    int len = snprintf(status, sizeof(status), "%.20s - %d lines %s",
            E.filename ? E.filename : "[No Name]", E.numrows,
            E.dirty ? "(modified)" : "");
    int rlen = snprintf(rstatus, sizeof(rstatus), "%d/%d", E.cy + 1, E.numrows);
    if (len > E.screencols) len = E.screencols;
    abuf_append(abuf, status, len);

    while (len < E.screencols) {
        if (E.screencols - len == rlen) {
            abuf_append(abuf, rstatus, rlen);
            break;
        }
        abuf_append(abuf, " ", 1);
        len++;
    }

    abuf_append(abuf, "\x1b[m", 3);
    abuf_append(abuf, "\r\n", 2);
}

void editor_draw_message_bar(struct AppendBuf *abuf) {
    abuf_append(abuf, "\x1b[K", 3);
    int msg_len = strlen(E.statusmsg);
    if (msg_len > E.screencols) msg_len = E.screencols;
    if (msg_len && time(NULL) - E.statusmsg_time < 5)
        abuf_append(abuf, E.statusmsg, msg_len);
}

void editor_refresh_screen(void) {
    editor_scroll();

    struct AppendBuf abuf = ABUF_INIT;

    abuf_append(&abuf, "\x1b[?25l", 6);
    abuf_append(&abuf, "\x1b[H", 3);

    editor_draw_rows(&abuf);
    editor_draw_status_bar(&abuf);
    editor_draw_message_bar(&abuf);

    char buf[32];
    snprintf(buf, sizeof(buf), "\x1b[%d;%dH", (E.cy - E.rowoff) + 1,
                                              (E.rx - E.coloff) + 1);
    abuf_append(&abuf, buf, strlen(buf));

    abuf_append(&abuf, "\x1b[?25h", 6);

    write(STDIN_FILENO, abuf.data, abuf.len);
    abuf_free(&abuf);
}

void editor_set_status_message(const char *fmt, ...) {
    va_list ap;
    va_start(ap, fmt);
    vsnprintf(E.statusmsg, sizeof(E.statusmsg), fmt, ap);
    va_end(ap);
    E.statusmsg_time = time(NULL);
}

/*** input ***/

char *editor_prompt(char *prompt, void (*callback) (char*, int)) {
    size_t buf_size = 128;
    char *buf = malloc(buf_size);

    size_t buf_len = 0;
    buf[0] = '\0';

    while (1) {
        editor_set_status_message(prompt, buf);
        editor_refresh_screen();

        int c = editor_read_key();
        if (c == DEL_KEY || c == CTRL_KEY('h') || c == BACKSPACE) {
            if (buf_len != 0) buf[--buf_len] = '\0';
        } else if (c == '\x1b') {
            editor_set_status_message("");
            if (callback) callback(buf, c);
            free(buf);
            return NULL;
        } else if (c == '\r') {
            if (buf_len != 0) {
                editor_set_status_message("");
                if (callback) callback(buf, c);
                return buf;
            }
        } else if (!iscntrl(c) && c < 128) {
            if (buf_len == buf_size - 1) {
                buf_size *= 2;
                buf = realloc(buf, buf_size);
            }
            buf[buf_len++] = c;
            buf[buf_len] = '\0';
        }

        if (callback) callback(buf, c);
    }
}

void editor_move_cursor(int key) {
    Row *row = (E.cy >= E.numrows) ? NULL : &E.row[E.cy];

    switch (key) {
        case ARROW_LEFT:
            if (E.cx != 0) {
                E.cx--;
            } else if (E.cy > 0) {
                E.cy--;
                E.cx = E.row[E.cy].size;
            }
            break;
        case ARROW_DOWN:
            if (E.cy < E.numrows)
                E.cy++;
            break;
        case ARROW_UP:
            if (E.cy != 0)
                E.cy--;
            break;
        case ARROW_RIGHT:
            if (row && E.cx < row->size) {
                E.cx++;
            } else if (row && E.cx == row->size) {
                E.cy++;
                E.cx = 0;
            }
            break;
    }

    row = (E.cy >= E.numrows) ? NULL : &E.row[E.cy];
    int rowlen = row ? row->size : 0;
    if (E.cx > rowlen)
        E.cx = rowlen;
}

void editor_process_keypress(void) {
    static int quit_times = KILO_QUIT_TIMES;

    int c = editor_read_key();

    switch (c) {
        case '\r':
            editor_insert_new_line();
            break;

        case CTRL_KEY('q'):
            if (E.dirty && quit_times > 0) {
                editor_set_status_message(
                    "WARNING!!! File has unsaved changes. "
                    "Press Ctrl-Q %d more times to quit.",
                    quit_times
                );
                quit_times--;
                return;
            }
            write(STDOUT_FILENO, "\x1b[2J", 4);
            write(STDOUT_FILENO, "\x1b[H", 3);
            exit(0);
            break;

        case CTRL_KEY('s'):
            editor_save();
            break;
        case HOME_KEY:
            E.cx = 0;
            break;
        case END_KEY:
            if (E.cy < E.numrows)
                E.cx = E.row[E.cy].size;
            break;

        case CTRL_KEY('f'):
            editor_find();
            break;

        case DEL_KEY:
            editor_move_cursor(ARROW_RIGHT);
            // fallthrough
        case BACKSPACE:
        case CTRL_KEY('h'):
            editor_del_char();
            break;

        case PAGE_UP:
        case PAGE_DOWN:
            {
                if (c == PAGE_UP) {
                    E.cy = E.rowoff;
                } else if (c == PAGE_DOWN) {
                    E.cy = E.rowoff + E.screenrows - 1;
                    if (E.cy > E.numrows) E.cy = E.numrows;
                }

                int times = E.screenrows;
                while (times--)
                    editor_move_cursor(c == PAGE_UP ? ARROW_UP : ARROW_DOWN);
            }
            break;
        case ARROW_LEFT:
        case ARROW_UP:
        case ARROW_DOWN:
        case ARROW_RIGHT:
            editor_move_cursor(c);
            break;

        case CTRL_KEY('l'):
        case '\x1b':
            break;

        default:
            editor_insert_char(c);
            break;
    }

    quit_times = KILO_QUIT_TIMES;
}

/*** init ***/

void init_editor(void) {
    E.cx = 0;
    E.cy = 0;
    E.rx = 0;
    E.rowoff = 0;
    E.coloff = 0;
    E.numrows = 0;
    E.row = NULL;
    E.dirty = 0;
    E.filename = NULL;
    E.statusmsg[0] = '\0';
    E.statusmsg_time = 0;

    if (get_window_size(&E.screenrows, &E.screencols) == -1)
        die("get_window_size");
    E.screenrows -= 2;
}

int main(int argc, char *argv[]) {
    enable_raw_mode();
    init_editor();

    if (argc >= 2) {
        editor_open(argv[1]);
    }

    editor_set_status_message("HELP: Ctrl-S = save | Ctrl-Q = quit | Ctrl-F = find");

    while (1) {
        editor_refresh_screen();
        editor_process_keypress();
    }
    return 0;
}
