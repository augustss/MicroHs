/* Copyright 2025 Lennart Augustsson
 * See LICENSE file for full license.
 */

/*
 * Find First Set
 * This macro should be defined.
 * It returns the number of the least significant bit that is set.
 * Numberings starts from 1.  If no bit is set, it should return 0.
 */
#if INTPTR_MAX == 0x7fffffff
/* 32 bit platform */
#if _POSIX_VERSION >= 200112L
#define FFS ffs
#endif  /* _POSIX_VERSION */

#else  /* INTPTR_MAX == 0x7fffffff */
/* 64 bit platform */
#if defined(__gnu_linux__) || defined(__APPLE__) || defined(__FreeBSD__)
/* Only some platforms have 64 bit ffsl function directly. */
#define FFS ffsl

#elif defined(__has_builtin)
#if __has_builtin(__builtin__ffsl)
#define FFS __builtin_ffsl
#endif  /* __has_builtin(__builtin_ffsl) */
#endif  /* if defined(...) */
#endif  /* INTPTR_MAX == 0x7fffffff */

#if defined(USE_WEB_INPUT)
#include <emscripten.h>
/*
 * When using mhs interactively on the web, we need a special input routine.
 */

/* A circular buffer.  The characters can arrive quicker than getraw consumes them.
 * This happens both for cursore keys and pasting.
 * We don't check for buffer overflow.
 */
#define IN_BUF_SIZE 16384
volatile int getraw_last_chars[IN_BUF_SIZE];
volatile int getraw_last_char_in = 0;
volatile int getraw_last_char_out = 0;
volatile int getraw_waiting = 0;

void handle_sigint(int);

/* JavaScript pushes the input character by calling this function */
EMSCRIPTEN_KEEPALIVE
void set_input_char(int c) {
  if (!getraw_waiting && c == 3) { /* CTRL-C */
    handle_sigint(0);
  }
  getraw_last_chars[getraw_last_char_in] = c;
  getraw_last_char_in = (getraw_last_char_in + 1) % IN_BUF_SIZE;
}

static int
getraw(void)
{
  getraw_waiting = 1;
  for(;;) {
    /* Busy-wait for a character to appear */
    if (getraw_last_char_in != getraw_last_char_out) {
      int ch = getraw_last_chars[getraw_last_char_out];
      getraw_last_char_out = (getraw_last_char_out + 1) % IN_BUF_SIZE;

      getraw_waiting = 0;
      return ch;
    }
    emscripten_sleep(10);
  }
}

/* While we are drawing we don't want to emscripten_sleep(),
 * because that may yield inside the renering loop and can
 * cause a premature transfer of the drawing buffer and flickering.
 */
int is_drawing = 0;

void c_begin_draw(void) {
    is_drawing = 1;
}

void c_end_draw(void) {
    is_drawing = 0;
}

/* Allow a thread switch in yield() */
#define YIELD_EXTRA do { if (!is_drawing) emscripten_sleep(0); } while(0)

// Create a C function called c_waitForFrame
// that executes the JavaScript inside the block.
EM_ASYNC_JS(void, c_waitForFrame, (), {
    return new Promise(function(resolve) {
        requestAnimationFrame(function() {
            resolve();
        });
    });
});

#else  /* USE_WEB_INPUT */

/*
 * Set the terminal in raw mode and read a single character.
 * Return this character, or -1 on any kind of failure.
 */
static int
getraw(void)
{
  struct termios old, new;
  unsigned char c;
  int r;

#if defined(USE_SYSTEM_RAW)

  /* For, e.g., execution with WASM in node the ioctl() doesn't seem to work */
  system("stty raw");
  r = read(0, &c, 1);
  system("stty -raw");

#else  /* USE_SYSTEM_RAW */

  if (tcgetattr(0, &old)) {
#if WANT_STDIO
    fprintf(stderr, "tcgetattr failed: errno=%d\n", errno);
#endif  /* WANT_STDIO */
    return -1;
  }
  new = old;
  new.c_iflag &= ~(IGNBRK | BRKINT | PARMRK | ISTRIP
                   | INLCR | IGNCR | ICRNL | IXON);
  new.c_oflag &= ~OPOST;
  new.c_lflag &= ~(ECHO | ECHONL | ICANON | ISIG | IEXTEN);
  new.c_cflag &= ~(CSIZE | PARENB);
  new.c_cflag |= CS8;
  if (tcsetattr(0, TCSANOW, &new)) {
#if WANT_STDIO
    fprintf(stderr, "tcsetattr 1 failed: errno=%d\n", errno);
#endif  /* WANT_STDIO */
    return -1;
  }
  r = read(0, &c, 1);
  if (tcsetattr(0, TCSANOW, &old)) {
#if WANT_STDIO
    fprintf(stderr, "tcsetattr 2 failed: errno=%d\n", errno);
#endif  /* WANT_STDIO */
    return -1;
  }

#endif  /* USE_SYSTEM_RAW */

  if (r == 1)
    return c;
  else {
#if WANT_STDIO
    fprintf(stderr, "read failed: errno=%d\n", errno);
#endif  /* WANT_STDIO */
    return -1;
  }
}
#endif  /* USE_WEB_INPUT */

/*
 * Get a raw input character.
 * If undefined, the default always returns -1
 */
#define GETRAW getraw

/*
 * Get time since some epoch in milliseconds.
 */
uintptr_t
gettimemilli(void)
{
  struct timeval tv;
  (void)gettimeofday(&tv, NULL);
  return (uintptr_t)(tv.tv_sec * 1000 + tv.tv_usec / 1000);
}
#define GETTIMEMILLI gettimemilli

/*
 * Create a unique file name.
 */
char*
tmpname(const char* pre, const char* suf)
{
  const char *tmpdir = getenv("TMPDIR");
  if (!tmpdir)
    tmpdir = "/tmp";

  char *path = malloc(PATH_MAX);
  if (!path)
    return 0;
  strncpy(path, tmpdir, PATH_MAX);
  strncat(path, "/", PATH_MAX - strlen(path) - 1);
  strncat(path, pre, PATH_MAX - strlen(path) - 1);
  strncat(path, "XXXXXX", PATH_MAX - strlen(path) - 1);
  strncat(path, suf, PATH_MAX - strlen(path) - 1);
  int fd = mkstemps(path, strlen(suf));
  if (fd < 0)
    return 0;
  close(fd);                    /* XXX Not great */
  return path;
}
#define TMPNAME tmpname

/*
 * Define CLOCK_INIT is there is a ticking clock.
 * CLOCK_INIT will be called for initializing the clock
 */
/* On Unix we just use gettimeofday() to get the clock,
 * so no initialization is needed.
 */
#define CLOCK_INIT() do { } while(0)
/* CLOCK_T is the type of the clock values. */
#define CLOCK_T int64_t       /* large enough for 290Myears */
/* CLOCK_GET returns the current clock in microseconds. */
#define CLOCK_GET clock_get
/* CLOCK_SLEEP sleeps some number of microseconds */
#define CLOCK_SLEEP usleep
CLOCK_T CLOCK_GET(void)
{
  struct timeval tv;
  (void)gettimeofday(&tv, 0);   /* this is very fast, about 16ns on an M4 MacMini */
  return (uint64_t)(tv.tv_sec * 1000000 + tv.tv_usec);
}

void
getcputime(long *sec, long *nsec)
{
#if WANT_TIME
  struct timespec ts;

  if (clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &ts) == 0) {
    *sec = ts.tv_sec;
    *nsec = ts.tv_nsec;
    return;
  }
#endif
  *sec = 0;
  *nsec = 0;
}
#define GETCPUTIME getcputime

#if WANT_KPERF

#if defined(__APPLE__) && defined(__MACH__) && (defined(__aarch64__) || defined(__arm64__))
#include "kperf-macos.c"
#else  /* apple-arm */
#if defined(__linux__) && (defined(__x86_64__) || defined(__amd64__))
#include "kperf-linux.c"
#else  /* linux */
/* No instruction counters */
int start_kperf(void) { return 1; }
uint64_t end_kperf(void) { return 0; }
#endif  /* linux */
#endif  /* apple-arm */

#endif  /* WANT_KPERF */

#include <stdlib.h>

#if defined(__linux__)
#include <unistd.h>
#include <limits.h>
#elif defined(__APPLE__) && defined(__MACH__)
#include <mach-o/dyld.h>
#include <limits.h>
#endif

/* Return path to executable as a null-terminated UTF-8 string. */
char*
get_executable_path(void)
{
#if defined(__linux__)
    return realpath("/proc/self/exe", NULL);

#elif defined(__APPLE__) && defined(__MACH__)
    uint32_t size = 0;
    _NSGetExecutablePath(NULL, &size);
    char *buf = malloc(size);
    if (!buf)
      return NULL;

    if (_NSGetExecutablePath(buf, &size) == 0) {
        char *canonical = realpath(buf, NULL);
        free(buf);
        return canonical;
    }
    free(buf);
    return NULL;

#else  /* Unsupported */
    return NULL;
#endif
}
#define GET_EXECUTABLE_PATH get_executable_path

#if defined(WANT_DIR)
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>
#include <fcntl.h>

/* Encoding shared with the Haskell side */
#define PERM_SEARCH 8
#define PERM_READ   4
#define PERM_WRITE  2
#define PERM_EXEC   1

int
get_permissions(const char *path)
{
  struct stat st;
  int perms = 0;

  if (stat(path, &st) == -1) {
    return -1;
  }

  if (st.st_mode & S_IRUSR)
    perms |= PERM_READ;
  if (st.st_mode & S_IWUSR)
    perms |= PERM_WRITE;
  if (st.st_mode & S_IXUSR) {
    if (S_ISDIR(st.st_mode)) {
      perms |= PERM_SEARCH;
    } else {
      perms |= PERM_EXEC;
    }
  }

  return perms;
}

int
set_permissions(const char *path, int perms)
{
  struct stat st;
  mode_t uperms = 0;
  int result;

  int fd = open(path, O_RDONLY | O_NONBLOCK);
  if (fd == -1) {
    return -1;
  }

  if (fstat(fd, &st) == -1) {
    close(fd);
    return -1;
  }

  if (perms & PERM_READ)
    uperms |= S_IRUSR;
  if (perms & PERM_WRITE)
    uperms |= S_IWUSR;
  if ((perms & PERM_EXEC) || (perms & PERM_SEARCH)) {
    uperms |= S_IXUSR;
  }

  uperms |= (st.st_mode & ~((mode_t)S_IRWXU));
  result = fchmod(fd, uperms);

  close(fd);
  return result;
}
#endif  /* WANT_DIR */
