/* Copyright 2026 Robert Krook
 * See LICENSE file for full license.
 */

/*
 * epoll backend for non-blocking IO polling (Linux only).
 * This file is #included into eval.c via io_poll_impl.c.
 *
 * A single TCP socket fd can have both a reader and a writer waiting on it
 * simultaneously — e.g. a receive loop blocked on EPOLLIN while a send path
 * is blocked on EPOLLOUT.  The original design used one cookie per fd with
 * EPOLL_CTL_ADD, which fails with EEXIST in that scenario.
 *
 * This version tracks separate read/write cookies per fd in a static table
 * and uses EPOLL_CTL_MOD when the fd is already in the epoll set, so both
 * directions stay live in a single epoll entry.
 */

#include <sys/epoll.h>
#include <fcntl.h>
#include <stdlib.h>

/* Per-fd waiter state. */
struct fd_state {
    void *read_cookie;   /* non-NULL while a thread is waiting for EPOLLIN  */
    void *write_cookie;  /* non-NULL while a thread is waiting for EPOLLOUT */
};

#define MAX_FDS 256
static struct fd_state fd_table[MAX_FDS]; /* zero-initialised by the C runtime */

static int epoll_fd   = -1;
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
    int fd = evs[i].data.fd;
    uint32_t got = evs[i].events;
    void *rc = NULL, *wc = NULL;

    /* Collect whichever cookies fired and clear them from the table. */
    if ((got & (EPOLLIN | EPOLLERR | EPOLLHUP)) && fd_table[fd].read_cookie) {
      rc = fd_table[fd].read_cookie;
      fd_table[fd].read_cookie = NULL;
      io_waiters--;
    }
    if ((got & (EPOLLOUT | EPOLLERR | EPOLLHUP)) && fd_table[fd].write_cookie) {
      wc = fd_table[fd].write_cookie;
      fd_table[fd].write_cookie = NULL;
      io_waiters--;
    }

    /* EPOLLONESHOT disabled the fd after firing. Re-arm for any remaining
       interest, or remove the fd from the set entirely. */
    uint32_t remaining = 0;
    if (fd_table[fd].read_cookie)  remaining |= EPOLLIN;
    if (fd_table[fd].write_cookie) remaining |= EPOLLOUT;

    if (remaining) {
      struct epoll_event ev = { .events = remaining | EPOLLONESHOT, .data.fd = fd };
      epoll_ctl(epoll_fd, EPOLL_CTL_MOD, fd, &ev);
    } else {
      epoll_ctl(epoll_fd, EPOLL_CTL_DEL, fd, NULL);
    }

    if (rc) on_ready(rc);
    if (wc) on_ready(wc);
  }
}

/* Register fd to call on_ready(cookie) when the requested event is ready.
   events is IO_POLL_READ or IO_POLL_WRITE.
   Uses EPOLL_CTL_MOD when the fd already has an interest registered in the
   other direction, so both directions coexist in a single epoll entry. */
void
io_register(int fd, int events, void *cookie)
{
  if (fd < 0 || fd >= MAX_FDS) ERR("io_register: fd out of range");

  int already_registered = (fd_table[fd].read_cookie  != NULL ||
                             fd_table[fd].write_cookie != NULL);

  if (events == IO_POLL_READ) {
    fd_table[fd].read_cookie  = cookie;
  } else {
    fd_table[fd].write_cookie = cookie;
  }

  uint32_t ev_flags = 0;
  if (fd_table[fd].read_cookie)  ev_flags |= EPOLLIN;
  if (fd_table[fd].write_cookie) ev_flags |= EPOLLOUT;
  ev_flags |= EPOLLONESHOT;

  struct epoll_event ev = { .events = ev_flags, .data.fd = fd };
  int op = already_registered ? EPOLL_CTL_MOD : EPOLL_CTL_ADD;
  if (epoll_ctl(epoll_fd, op, fd, &ev) < 0) {
    ERR("io_register epoll_ctl failed");
  }
  io_waiters++;
}

int
io_waiter_count(void)
{
  return io_waiters;
}
