/* Copyright 2026 Robert Krook
 * See LICENSE file for full license.
 */

/*
 * OS-agnostic interface for non-blocking IO polling.
 * Platform-specific implementations live in <platform>/io_poll_impl.c and
 * are included into eval.c via #include "io_poll_impl.c".
 *
 */

#define IO_POLL_READ  1
#define IO_POLL_WRITE 2

struct mthread;  /* forward declaration; full definition is in eval.c */

void io_init(void);
void io_poll(int timeout_ms);
void io_register(int fd, int events, struct mthread *mt);
int  io_waiter_count(void);
