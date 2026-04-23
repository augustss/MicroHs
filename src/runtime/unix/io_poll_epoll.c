/* Copyright 2026 Robert Krook
 * See LICENSE file for full license.
 */

/*
 * epoll backend for non-blocking IO polling (Linux only).
 * This file is #included into eval.c via io_poll_impl.c, so it has
 * access to all definitions in eval.c (struct mthread, add_runq_tail, etc.).
 */

#include <sys/epoll.h>
#include <fcntl.h>

static int epoll_fd  = -1;
static int io_waiters = 0;

void
io_init(void)
{
  epoll_fd = epoll_create1(EPOLL_CLOEXEC);
  if (epoll_fd < 0) {
    ERR("epoll_create1 failed");
  }
}

/* If timeout_ms is 0 this is a non-blocking check; otherwise it blocks up to
   timeout_ms milliseconds (or indefinitely when timeout_ms == -1). */
void
io_poll(int timeout_ms)
{
  struct epoll_event evs[64];
  int n = epoll_wait(epoll_fd, evs, 64, timeout_ms);
  for (int i = 0; i < n; i++) {
    struct mthread *mt = evs[i].data.ptr;
    /* EPOLLONESHOT disables but does not remove the fd; delete it now so
       that a subsequent io_register on the same fd can use EPOLL_CTL_ADD. */
    epoll_ctl(epoll_fd, EPOLL_CTL_DEL, mt->mt_fd, NULL);
    mt->mt_fd = -2; /* signal "already woken" to the eval loop */
    io_waiters--;
    add_runq_tail(mt);
  }
}

/* Register fd to wake mt when the requested event is ready.
   events is IO_POLL_READ or IO_POLL_WRITE. */
void
io_register(int fd, int events, struct mthread *mt)
{
  uint32_t ev_flags = (events == IO_POLL_READ ? EPOLLIN : EPOLLOUT) | EPOLLONESHOT;
  struct epoll_event ev = { .events = ev_flags, .data.ptr = mt };
  if (epoll_ctl(epoll_fd, EPOLL_CTL_ADD, fd, &ev) < 0) {
    ERR("epoll_ctl ADD failed");
  }
  io_waiters++;
}

int
io_waiter_count(void)
{
  return io_waiters;
}
