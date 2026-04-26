/* Copyright 2026 Robert Krook
 * See LICENSE file for full license.
 */

/*
 * epoll backend for non-blocking IO polling (Linux only).
 * This file is #included into eval.c via io_poll_impl.c.
 */

#include <sys/epoll.h>
#include <fcntl.h>
#include <stdlib.h>

/*
This is the user data that we register with epoll. When an event fires, we can retrieve it.

We need to be able to reference both the void *cookie and the file descriptor it is waiting
for. We need to void *cookie to call the on_ready callback on, and we need the file descriptor
so that we can unregister it from epoll.

*/
struct io_entry { void *cookie; int fd; };

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
io_poll(int timeout_ms, void (*on_ready)(void *cookie))
{
  struct epoll_event evs[64];
  int n = epoll_wait(epoll_fd, evs, 64, timeout_ms);
  for (int i = 0; i < n; i++) {
    struct io_entry *e = evs[i].data.ptr;
    /* EPOLLONESHOT disables but does not remove the fd; delete it now so
       that a subsequent io_register on the same fd can use EPOLL_CTL_ADD. */
    epoll_ctl(epoll_fd, EPOLL_CTL_DEL, e->fd, NULL);
    io_waiters--;
    on_ready(e->cookie);
    free(e);
  }
}

/* Register fd to call on_ready(cookie) when the requested event is ready.
   events is IO_POLL_READ or IO_POLL_WRITE. */
void
io_register(int fd, int events, void *cookie)
{
  struct io_entry *e = malloc(sizeof *e);
  if (!e) ERR("io_register malloc failed");
  e->cookie = cookie;
  e->fd     = fd;
  uint32_t ev_flags = (events == IO_POLL_READ ? EPOLLIN : EPOLLOUT) | EPOLLONESHOT;
  struct epoll_event ev = { .events = ev_flags, .data.ptr = e };
  if (epoll_ctl(epoll_fd, EPOLL_CTL_ADD, fd, &ev) < 0) {
    free(e);
    ERR("epoll_ctl ADD failed");
  }
  io_waiters++;
}

int
io_waiter_count(void)
{
  return io_waiters;
}
