/*** imports ***/

#include <ctype.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/ioctl.h>
#include <termios.h>
#include <unistd.h>

/*** defines ***/

#define VERSION "0.0.1"

#define CTRL_KEY(k) ((k) & 0x1f)

/*** data ***/

struct EditorConfig {
    int cx, cy;
    int screenrows;
    int screencols;
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

char editor_read_key(void) {
    int nread;
    char c;
    while ((nread = read(STDIN_FILENO, &c, 1)) != 1) {
        if (nread == -1 && errno != EAGAIN)
            die("read");
    }
    return c;
}

int get_cursor_position(int *rows, int *cols) {
    char buf[32];
    unsigned int i = 0;
    if (write(STDOUT_FILENO, "\x1b[6n", 4) != 4) return -1;

    printf("\r\n");

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
        if (y == E.screenrows / 3) {
            char welcome[80];
            int welcomelen = snprintf(
                welcome,
                sizeof(welcome),
                "Kilo editor -- version %s",
                VERSION
            );
            if (welcomelen > E.screencols)
                welcomelen = E.screencols;
            int padding = (E.screencols - welcomelen) / 2;
            if (padding) {
                abuf_append(abuf, "~", 1);
                padding--;
            }
            while (padding--)
                abuf_append(abuf, " ", 1);

            abuf_append(abuf, welcome, welcomelen);
        } else {
            abuf_append(abuf, "~", 1);
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

    abuf_append(&abuf, "\x1b[H", 3);
    abuf_append(&abuf, "\x1b[?25h", 6);

    write(STDIN_FILENO, abuf.data, abuf.len);
    abuf_free(&abuf);
}

/*** input ***/

void editor_process_keypress(void) {
    char c = editor_read_key();

    switch (c) {
        case CTRL_KEY('q'):
            write(STDOUT_FILENO, "\x1b[2J", 4);
            write(STDOUT_FILENO, "\x1b[H", 3);
            exit(0);
            break;
    }
}

/*** init ***/

void init_editor(void) {
    E.cx = 0;
    E.cy = 0;

    if (get_window_size(&E.screenrows, &E.screencols) == -1)
        die("get_window_size");
}

int main(void) {
    enable_raw_mode();
    init_editor();

    while (1) {
        editor_refresh_screen();
        editor_process_keypress();
    }
    return 0;
}
