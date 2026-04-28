/* Copyright 2026 Robert Krook
 * See LICENSE file for full license.
 */

/*
 * kqueue backend for non-blocking IO polling (macOS / BSD).
 * This file is #included into eval.c via io_poll_impl.c.
 */

#include <sys/types.h>
#include <sys/event.h>
#include <sys/time.h>

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
    io_waiters--;
    on_ready((void *)evs[i].udata);
  }
}

/* Register fd to call on_ready(cookie) when the requested event is ready.
   events is IO_POLL_READ or IO_POLL_WRITE. */
void
io_register(int fd, int events, void *cookie)
{
  struct kevent ev;
  int16_t filter = (events == IO_POLL_READ) ? EVFILT_READ : EVFILT_WRITE;
  EV_SET(&ev, fd, filter, EV_ADD | EV_ONESHOT, 0, 0, cookie);
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
