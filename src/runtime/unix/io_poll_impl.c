/* Copyright 2026 Robert Krook
 * See LICENSE file for full license.
 */

/*
 * Selects the platform-specific IO poll backend for Unix targets.
 */

#if defined(__linux__)
#include "io_poll_epoll.c"
#elif defined(__APPLE__) && defined(__MACH__)
#include "io_poll_kqueue.c"
#else
#error "No IO poll backend available for this platform"
#endif
