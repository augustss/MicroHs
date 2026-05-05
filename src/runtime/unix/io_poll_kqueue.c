/* Copyright 2026 Robert Krook
 * See LICENSE file for full license.
 */

/*
 * kqueue backend for non-blocking IO polling (macOS / BSD).
 * This file is #included into eval.c via io_poll_impl.c.
 *
 * kqueue tracks EVFILT_READ and EVFILT_WRITE as independent (fd, filter)
 * entries, so concurrent read and write waiters on the same fd do not
 * conflict at the kqueue level.  We still maintain a per-fd cookie table
 * (mirroring the epoll backend) so that io_poll can dispatch to the correct
 * waiting thread without storing the cookie in udata and casting.
 */

#include <sys/types.h>
#include <sys/event.h>
#include <sys/time.h>

/* Per-fd waiter state. */
struct fd_state {
    void *read_cookie;   /* non-NULL while a thread is waiting for EVFILT_READ  */
    void *write_cookie;  /* non-NULL while a thread is waiting for EVFILT_WRITE */
};

#define MAX_FDS 256
static struct fd_state fd_table[MAX_FDS]; /* zero-initialised by the C runtime */

static int kqueue_fd  = -1;
static int io_waiters = 0;

void
io_init(void)
{
  kqueue_fd = kqueue();
  if (kqueue_fd < 0) {
    ERR("kqueue failed");
  }
}

/* If timeout_ms is 0 this is a non-blocking check; otherwise it blocks up to
   timeout_ms milliseconds (or indefinitely when timeout_ms == -1). */
void
io_poll(int timeout_ms, void (*on_ready)(void *cookie))
{
  struct kevent evs[64];
  struct timespec ts;
  struct timespec *tsp = NULL;
  if (timeout_ms >= 0) {
    ts.tv_sec  = timeout_ms / 1000;
    ts.tv_nsec = (long)(timeout_ms % 1000) * 1000000L;
    tsp = &ts;
  }
  int n = kevent(kqueue_fd, NULL, 0, evs, 64, tsp);
  for (int i = 0; i < n; i++) {
    int fd = (int)evs[i].ident;
    void *cookie = NULL;
    if (evs[i].filter == EVFILT_READ) {
      cookie = fd_table[fd].read_cookie;
      fd_table[fd].read_cookie = NULL;
    } else {
      cookie = fd_table[fd].write_cookie;
      fd_table[fd].write_cookie = NULL;
    }
    io_waiters--;
    if (cookie) on_ready(cookie);
  }
}

/* Register fd to call on_ready(cookie) when the requested event is ready.
   events is IO_POLL_READ or IO_POLL_WRITE. */
void
io_register(int fd, int events, void *cookie)
{
  if (fd < 0 || fd >= MAX_FDS) ERR("io_register: fd out of range");

  struct kevent ev;
  int16_t filter;
  if (events == IO_POLL_READ) {
    fd_table[fd].read_cookie  = cookie;
    filter = EVFILT_READ;
  } else {
    fd_table[fd].write_cookie = cookie;
    filter = EVFILT_WRITE;
  }

  EV_SET(&ev, fd, filter, EV_ADD | EV_ONESHOT, 0, 0, 0);
  if (kevent(kqueue_fd, &ev, 1, NULL, 0, NULL) < 0) {
    ERR("kevent ADD failed");
  }
  io_waiters++;
}

int
io_waiter_count(void)
{
  return io_waiters;
}
