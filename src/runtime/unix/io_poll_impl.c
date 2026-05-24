/* Copyright 2026 Robert Krook
 * See LICENSE file for full license.
 */

/*
 * IO poll backend for Unix.
 *
 * Currently Linux only, using epoll. When a second implementation for MACOS is
 * available, do some tricks here to dispatch the correct one.
 *
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

#include <fcntl.h>
#include <stdlib.h>

/**************************************************************************/
#if defined(ISLINUX)
#include <sys/epoll.h>
#elif defined(ISMACOS)

#include <sys/types.h>
#include <sys/event.h>
#include <sys/time.h>
#include <unistd.h>
#include <fcntl.h>
#include <stdlib.h>
#include <errno.h>
#include <stdint.h>
#include <string.h>

/* Emulated Epoll Definitions */
#define EPOLL_CTL_ADD 1
#define EPOLL_CTL_DEL 2
#define EPOLL_CTL_MOD 3

#define EPOLLIN      0x0001
#define EPOLLOUT     0x0004
#define EPOLLERR     0x0008
#define EPOLLHUP     0x0010
#define EPOLLONESHOT (1U << 30)

#define EPOLL_CLOEXEC O_CLOEXEC

typedef union epoll_data {
  void    *ptr;
  int      fd;
  uint32_t u32;
  uint64_t u64;
} epoll_data_t;

struct epoll_event {
  uint32_t     events;
  epoll_data_t data;
};

/* Emulated Functions */

int
epoll_create1(int flags)
{
  int kq = kqueue();
  if (kq == -1) {
    return -1;
  }

  if (flags & EPOLL_CLOEXEC) {
    if (fcntl(kq, F_SETFD, FD_CLOEXEC) == -1) {
      close(kq);
      return -1;
    }
  }
  return kq;
}

int
epoll_ctl(int epfd, int op, int fd, struct epoll_event *event)
{
  struct kevent ke[2];
  int nchanges = 0;
  uint16_t kqueue_flags = 0;

  if (op == EPOLL_CTL_DEL) {
    // For deletion, explicitly delete both potential filters
    EV_SET(&ke[0], fd, EVFILT_READ, EV_DELETE, 0, 0, NULL);
    EV_SET(&ke[1], fd, EVFILT_WRITE, EV_DELETE, 0, 0, NULL);

    // kevent returns failure if we delete a filter that wasn't registered.
    // We run them separately or safely ignore ENOENT errors.
    kevent(epfd, &ke[0], 1, NULL, 0, NULL);
    kevent(epfd, &ke[1], 1, NULL, 0, NULL);
    return 0;
  }

  if (op == EPOLL_CTL_ADD || op == EPOLL_CTL_MOD) {
    // If modifying, we clear previous registrations by deleting them first
    if (op == EPOLL_CTL_MOD) {
      EV_SET(&ke[0], fd, EVFILT_READ, EV_DELETE, 0, 0, NULL);
      EV_SET(&ke[1], fd, EVFILT_WRITE, EV_DELETE, 0, 0, NULL);
      kevent(epfd, &ke[0], 1, NULL, 0, NULL);
      kevent(epfd, &ke[1], 1, NULL, 0, NULL);
    }

    // Determine kqueue action modifiers
    kqueue_flags = EV_ADD | EV_ENABLE;
    if (event->events & EPOLLONESHOT) {
      kqueue_flags |= EV_ONESHOT;
    }

    // Allocate filters based on requested interest flags
    if (event->events & EPOLLIN) {
      // Allocate a user-data copy to pass along inside kevent's udata pointer
      struct epoll_event *udata = malloc(sizeof(struct epoll_event));
      if (!udata) return -1;
      *udata = *event;

      EV_SET(&ke[nchanges++], fd, EVFILT_READ, kqueue_flags, 0, 0, udata);
    }
    if (event->events & EPOLLOUT) {
      struct epoll_event *udata = malloc(sizeof(struct epoll_event));
      if (!udata) return -1;
      *udata = *event;

      EV_SET(&ke[nchanges++], fd, EVFILT_WRITE, kqueue_flags, 0, 0, udata);
    }

    if (nchanges > 0) {
      if (kevent(epfd, ke, nchanges, NULL, 0, NULL) == -1) {
        return -1;
      }
    }
    return 0;
  }

  errno = EINVAL;
  return -1;
}

int
epoll_wait(int epfd, struct epoll_event *events, int maxevents, int timeout)
{
  struct kevent *kq_events = malloc(sizeof(struct kevent) * maxevents);
  if (!kq_events) {
    return -1;
  }

  struct timespec ts;
  struct timespec *timeout_ptr = NULL;

  if (timeout >= 0) {
    ts.tv_sec = timeout / 1000;
    ts.tv_nsec = (timeout % 1000) * 1000000;
    timeout_ptr = &ts;
  }

  int n_events = kevent(epfd, NULL, 0, kq_events, maxevents, timeout_ptr);
  if (n_events <= 0) {
    free(kq_events);
    return n_events; // Returns 0 on timeout, -1 on error
  }

  for (int i = 0; i < n_events; i++) {
    struct epoll_event *udata = (struct epoll_event *)kq_events[i].udata;

    // Restore user data configuration
    events[i].data = udata->data;
    events[i].events = 0;

    // Translate kqueue status events back to Epoll flag interpretations
    if (kq_events[i].filter == EVFILT_READ)  events[i].events |= EPOLLIN;
    if (kq_events[i].filter == EVFILT_WRITE) events[i].events |= EPOLLOUT;

    if (kq_events[i].flags & EV_ERROR)       events[i].events |= EPOLLERR;
    if (kq_events[i].flags & EV_EOF)         events[i].events |= EPOLLHUP;

    // Clean up heap allocation if EV_ONESHOT triggered and consumed the filter
    if (udata && (kq_events[i].flags & EV_ONESHOT)) {
      free(udata);
    }
  }

  free(kq_events);
  return n_events;
}

#elif defined(__EMSCRIPTEN__)

#include <poll.h>
#include <unistd.h>
#include <fcntl.h>
#include <stdlib.h>
#include <errno.h>
#include <stdint.h>
#include <string.h>

/* Emulated Epoll Definitions */
#define EPOLL_CTL_ADD 1
#define EPOLL_CTL_DEL 2
#define EPOLL_CTL_MOD 3

#define EPOLLIN      0x0001
#define EPOLLOUT     0x0004
#define EPOLLERR     0x0008
#define EPOLLHUP     0x0010
#define EPOLLONESHOT (1U << 30)

#define EPOLL_CLOEXEC O_CLOEXEC

typedef union epoll_data {
    void    *ptr;
    int      fd;
    uint32_t u32;
    uint64_t u64;
} epoll_data_t;

struct epoll_event {
    uint32_t     events;
    epoll_data_t data;
};

/* Internal Tracking State */
typedef struct {
    struct pollfd *fds;       /* Array of descriptors passed to poll() */
    struct epoll_event *events; /* Matching user data array for epoll_data_t */
    int count;                /* Current number of monitored descriptors */
    int capacity;             /* Allocated size of the tracking arrays */
} EpollState;

/* Helper to access state hidden behind the file descriptor integer */
static EpollState *epoll_states[1024]; // Basic tracking registry

int
epoll_create1(int flags)
{
  // We create a fake file descriptor using a pipe just to give the caller a valid tracking handle
  int pipefds[2];
  if (pipe(pipefds) == -1) return -1;
  close(pipefds[1]); // Close write end, keep read end as our virtual "epfd"
  int epfd = pipefds[0];

  if (flags & EPOLL_CLOEXEC) {
    fcntl(epfd, F_SETFD, FD_CLOEXEC);
  }

  // Allocate internal interest list state
  EpollState *state = malloc(sizeof(EpollState));
  if (!state) {
    close(epfd);
    return -1;
  }
  state->count = 0;
  state->capacity = 16;
  state->fds = malloc(sizeof(struct pollfd) * state->capacity);
  state->events = malloc(sizeof(struct epoll_event) * state->capacity);

  if (!state->fds || !state->events) {
    free(state->fds);
    free(state->events);
    free(state);
    close(epfd);
    return -1;
  }

  epoll_states[epfd] = state;
  return epfd;
}

int
epoll_ctl(int epfd, int op, int fd, struct epoll_event *event)
{
  EpollState *state = epoll_states[epfd];
  if (!state) {
    errno = EBADF;
    return -1;
  }

  // Find if the fd is already in our list
  int index = -1;
  for (int i = 0; i < state->count; i++) {
    if (state->fds[i].fd == fd) {
      index = i;
      break;
    }
  }

  if (op == EPOLL_CTL_ADD) {
    if (index != -1) {
      errno = EEXIST;
      return -1;
    }

    // Resize array if capacity is reached
    if (state->count >= state->capacity) {
      state->capacity *= 2;
      state->fds = realloc(state->fds, sizeof(struct pollfd) * state->capacity);
      state->events = realloc(state->events, sizeof(struct epoll_event) * state->capacity);
    }

    // Map epoll events to poll events
    short poll_events = 0;
    if (event->events & EPOLLIN)  poll_events |= POLLIN;
    if (event->events & EPOLLOUT) poll_events |= POLLOUT;

    state->fds[state->count].fd = fd;
    state->fds[state->count].events = poll_events;
    state->fds[state->count].revents = 0;
    state->events[state->count] = *event; // Save original user data & oneshot configuration
    state->count++;
    return 0;
  }

  if (op == EPOLL_CTL_MOD) {
    if (index == -1) {
      errno = ENOENT;
      return -1;
    }

    short poll_events = 0;
    if (event->events & EPOLLIN)  poll_events |= POLLIN;
    if (event->events & EPOLLOUT) poll_events |= POLLOUT;

    state->fds[index].events = poll_events;
    state->events[index] = *event;
    return 0;
  }

  if (op == EPOLL_CTL_DEL) {
    if (index == -1) {
      errno = ENOENT;
      return -1;
    }

    // Shift remaining items forward to close the gap
    for (int i = index; i < state->count - 1; i++) {
      state->fds[i] = state->fds[i + 1];
      state->events[i] = state->events[i + 1];
    }
    state->count--;
    return 0;
  }

  errno = EINVAL;
  return -1;
}

int
epoll_wait(int epfd, struct epoll_event *events, int maxevents, int timeout)
{
  EpollState *state = epoll_states[epfd];
  if (!state) {
    errno = EBADF;
    return -1;
  }

  if (state->count == 0) {
    // If there's nothing to poll, sleep manually if a timeout is specified
    if (timeout > 0) usleep(timeout * 1000);
    return 0;
  }

  // Call native poll
  int ready = poll(state->fds, state->count, timeout);
  if (ready <= 0) {
    return ready; // Returns 0 on timeout, -1 on error
  }

  int num_events = 0;
  for (int i = 0; i < state->count && num_events < maxevents; i++) {
    if (state->fds[i].revents != 0) {
      // Restore user data structure mapping
      events[num_events].data = state->events[i].data;
      events[num_events].events = 0;

      // Translate poll outcome flags back to epoll flags
      if (state->fds[i].revents & POLLIN)  events[num_events].events |= EPOLLIN;
      if (state->fds[i].revents & POLLOUT) events[num_events].events |= EPOLLOUT;
      if (state->fds[i].revents & POLLERR) events[num_events].events |= EPOLLERR;
      if (state->fds[i].revents & POLLHUP) events[num_events].events |= EPOLLHUP;

      // Emulate EPOLLONESHOT behavior
      if (state->events[i].events & EPOLLONESHOT) {
        // To disarm the descriptor under poll, we set events to 0
        state->fds[i].events = 0;
      }

      num_events++;
    }
  }

  return num_events;
}

#endif /* defined(__EMSCRIPTEN__) */


/**************************************************************************/


/* Per-fd waiter state. */
struct fd_state {
    void *read_cookie;   /* non-NULL while a thread is waiting for EPOLLIN  */
    void *write_cookie;  /* non-NULL while a thread is waiting for EPOLLOUT */
};

/* XXX This needs to be replace by something more dynamic and safe. */
#define MAX_FDS 256
static struct fd_state fd_table[MAX_FDS];

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
 * timeout_ms milliseconds (or indefinitely when timeout_ms == -1).
 */
void
io_poll(int timeout_ms, void (*on_ready)(void *cookie))
{
  struct epoll_event evs[64];
  int n = epoll_wait(epoll_fd, evs, sizeof(evs) / sizeof (evs[0]), timeout_ms);
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
     * interest, or remove the fd from the set entirely.
     */
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
 * events is IO_POLL_READ or IO_POLL_WRITE.
 * Uses EPOLL_CTL_MOD when the fd already has an interest registered in the
 * other direction, so both directions coexist in a single epoll entry.
 */
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
