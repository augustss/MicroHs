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

#include "io_poll_epoll.c"
