/*** imports ***/

#define _DEFAULT_SOURCE
#define _BSD_SOURCE
#define _GNU_SOURCE

#include <ctype.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/ioctl.h>
#include <sys/types.h>
#include <termios.h>
#include <unistd.h>

/*** defines ***/

#define VERSION "0.0.1"

#define CTRL_KEY(k) ((k) & 0x1f)

enum EditorKey {
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

/*** data ***/

typedef struct Row {
    int size;
    char *chars;
} Row;

struct EditorConfig {
    int cx, cy;
    int rowoff;
    int screenrows;
    int screencols;
    int numrows;
    Row *row;
    struct termios orig_termios;
};

struct EditorConfig E;

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

/*** row operations ***/

void editor_append_row(char *s, size_t len) {
    E.row = realloc(E.row, sizeof(Row) * (E.numrows + 1));

    int at = E.numrows;
    E.row[at].size = len;
    E.row[at].chars = malloc(len + 1);
    memcpy(E.row[at].chars, s, len);
    E.row[at].chars[len] = '\0';
    E.numrows++;
}

/*** file i/o ***/

void editor_open(char *filename) {
    FILE *fp = fopen(filename, "r");
    if (!fp) die("fopen");

    char *line = NULL;
    size_t line_cap = 0;
    ssize_t line_len;

    while ((line_len = getline(&line, &line_cap, fp)) != -1) {
        while (line_len > 0 && (line[line_len - 1] == '\n' ||
                                line[line_len - 1] == '\r'))
            line_len--;
        editor_append_row(line, line_len);
    }

    free(line);
    fclose(fp);
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
            int len = E.row[filerow].size;
            if (len > E.screencols)
                len = E.screencols;
            abuf_append(abuf, E.row[filerow].chars, len);
        }

        abuf_append(abuf, "\x1b[K", 3);
        if (y < E.screenrows - 1)
            abuf_append(abuf, "\r\n", 2);
    }
}

void editor_refresh_screen(void) {
    struct AppendBuf abuf = ABUF_INIT;

    abuf_append(&abuf, "\x1b[?25l", 6);
    abuf_append(&abuf, "\x1b[H", 3);

    editor_draw_rows(&abuf);

    char buf[32];
    snprintf(buf, sizeof(buf), "\x1b[%d;%dH", E.cy + 1, E.cx + 1);
    abuf_append(&abuf, buf, strlen(buf));

    abuf_append(&abuf, "\x1b[?25h", 6);

    write(STDIN_FILENO, abuf.data, abuf.len);
    abuf_free(&abuf);
}

/*** input ***/

void editor_move_cursor(int key) {
    switch (key) {
        case ARROW_LEFT:
            if (E.cx != 0)
                E.cx--;
            break;
        case ARROW_DOWN:
            if (E.cy != E.screenrows - 1)
                E.cy++;
            break;
        case ARROW_UP:
            if (E.cy != 0)
                E.cy--;
            break;
        case ARROW_RIGHT:
            if (E.cx != E.screencols - 1)
                E.cx++;
            break;
    }
}

void editor_process_keypress(void) {
    int c = editor_read_key();

    switch (c) {
        case CTRL_KEY('q'):
            write(STDOUT_FILENO, "\x1b[2J", 4);
            write(STDOUT_FILENO, "\x1b[H", 3);
            exit(0);
            break;
        case HOME_KEY:
            E.cx = 0;
            break;
        case END_KEY:
            E.cx = E.screencols - 1;
            break;
        case PAGE_UP:
        case PAGE_DOWN:
            {
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
    }
}

/*** init ***/

void init_editor(void) {
    E.cx = 0;
    E.cy = 0;
    E.rowoff = 0;
    E.numrows = 0;
    E.row = NULL;

    if (get_window_size(&E.screenrows, &E.screencols) == -1)
        die("get_window_size");
}

int main(int argc, char *argv[]) {
    enable_raw_mode();
    init_editor();

    if (argc >= 2) {
        editor_open(argv[1]);
    }

    while (1) {
        editor_refresh_screen();
        editor_process_keypress();
    }
    return 0;
}
