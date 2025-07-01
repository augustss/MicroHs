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

/*
 * Set the terminal in raw mode and read a single character.
 * Return this character, or -1 on any kind of failure.
 */
static int
getraw(void)
{
  struct termios old, new;
  char c;
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
 * This is a very dodgy implementation.
 * XXX This functions drags in a lot of functionality.
 */
char*
tmpname(const char* pre, const char* suf)
{
  int pid = (int)getpid();
  char *tmp = getenv("TMPDIR");
  const size_t PID_DIGITS = 10;
  if (!tmp)
    tmp = "/tmp";
  char *s = malloc(strlen(tmp) + 1 + strlen(pre) + PID_DIGITS + strlen(suf) + 1);
  /* This might loop forever.  See if I care. :) */
  for(;;) {
    strcpy(s, tmp);
    strcat(s, "/");
    strcat(s, pre);
    /* Insert PID_DIGITS digits of the PID */
    char *p = s + strlen(s) + PID_DIGITS;
    *p-- = 0;
    for(int i = 0; i < PID_DIGITS; i++) {
      *p-- = pid % 10 + '0';
      pid /= 10;
    }
    strcat(s, suf);
    /* The file name is ready, do a quick if check that we can create it */
    int fd = open(s, O_CREAT | O_EXCL, 0600);
    if (fd >= 0) {
      close(fd);                /* Close it again */
      return s;
    }
    pid++;                      /* try with a different pid */
  }
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
