// Code from ChatGPT-5
// kperf_inst_stable.c  (Linux version)

#define _GNU_SOURCE
#include <errno.h>
#include <inttypes.h>
#include <linux/perf_event.h>
#define __USE_GNU
#include <sched.h>
#undef __USE_GNU
#include <signal.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/ioctl.h>
#include <sys/resource.h>
#include <sys/syscall.h>
#include <sys/types.h>
#include <unistd.h>

// Soft pin to a single CPU to reduce migration noise.
static void pin_to_cpu(int cpu) {
  cpu_set_t set;
  CPU_ZERO(&set);
  CPU_SET(cpu, &set);
  (void)sched_setaffinity(0, sizeof(set), &set); /* ignore error */
}

// Optional: try to bump priority a bit (may require CAP_SYS_NICE; ignore errors).
static void try_bump_priority(void) {
  (void)setpriority(PRIO_PROCESS, 0, -5); /* ignore error */
}

// Open a per-thread perf event for "instructions retired".
static int open_instr_counter(void) {
  struct perf_event_attr attr;
  memset(&attr, 0, sizeof(attr));
  attr.type = PERF_TYPE_HARDWARE;
  attr.size = sizeof(attr);
  attr.config = PERF_COUNT_HW_INSTRUCTIONS;
  attr.disabled = 1;                 // start disabled
  attr.exclude_kernel = 1;           // count only user-space
  attr.exclude_hv = 1;               // exclude hypervisor
  attr.exclude_idle = 1;             // (mostly relevant for system-wide)
  attr.read_format = PERF_FORMAT_TOTAL_TIME_ENABLED | PERF_FORMAT_TOTAL_TIME_RUNNING;

  // Per-thread mode: pid = 0, cpu = -1, current thread, any CPU
  int fd = (int)syscall(__NR_perf_event_open, &attr, 0, -1, -1, 0);
  if (fd < 0) {
    perror("perf_event_open");
    fprintf(stderr,
      "Hint: you might need: sudo sysctl kernel.perf_event_paranoid=1 (or 0)\n");
  }
  return fd;
}

// Read counter
static uint64_t read_counter(int fd) {
  struct {
    uint64_t value;
    uint64_t time_enabled;
    uint64_t time_running;
  } data;
  ssize_t n = read(fd, &data, sizeof(data));
  if (n != (ssize_t)sizeof(data)) {
    perror("read(perf)");
    return 0;
  }
  return data.value;
}

static int kperf_fd = -1;

// Measure median (over 'reps') of (instructions per call), averaging each rep over N calls.
static int
start_kperf(void) {
  // Stabilizers
  pin_to_cpu(0);
  try_bump_priority();

  int fd = open_instr_counter();
  if (fd < 0)
    return 0;
  if (ioctl(fd, PERF_EVENT_IOC_RESET, 0) != 0)
    perror("ioctl RESET failed");
  if (ioctl(fd, PERF_EVENT_IOC_ENABLE, 0) != 0)
    perror("ioctl ENABLE failed");

  kperf_fd = fd;
  return 1;
}

static uint64_t
end_kperf(void)
{
    if (ioctl(kperf_fd, PERF_EVENT_IOC_DISABLE, 0) != 0)
      perror("ioctl DISABLE failed");

    if (kperf_fd >= 0) {
      uint64_t instr = read_counter(kperf_fd);

      close(kperf_fd);
      return instr;
    } else {
      return 0;
    }
}
