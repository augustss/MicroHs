// Code originally from ChatGPT5

#define _DARWIN_C_SOURCE
#include <dlfcn.h>
#include <mach/mach.h>
#include <mach/thread_policy.h>
#include <pthread.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// ---- kperf symbols (resolved at runtime) ----
typedef int  (*kpc_set_counting_t)(uint32_t classes);
typedef int  (*kpc_set_thread_counting_t)(uint32_t classes);
typedef uint32_t  (*kpc_get_counter_count_t)(uint32_t classes);
typedef int  (*kpc_get_thread_counters_t)(uint32_t tid, uint32_t buf_count, uint64_t *buf);
typedef int  (*kpc_force_all_ctrs_set_t)(int val);
typedef int  (*kpc_force_all_ctrs_get_t)(int *val);

static kpc_set_counting_t          kpc_set_counting;
static kpc_set_thread_counting_t   kpc_set_thread_counting;
static kpc_get_counter_count_t     kpc_get_counter_count;
static kpc_get_thread_counters_t   kpc_get_thread_counters;
static kpc_force_all_ctrs_set_t    kpc_force_all_ctrs_set;
static kpc_force_all_ctrs_get_t    kpc_force_all_ctrs_get;

enum { KPC_CLASS_FIXED = 0 };
#define KPC_CLASS_FIXED_MASK (1u << KPC_CLASS_FIXED)

static int load_kperf(void) {
  const char *path = "/System/Library/PrivateFrameworks/kperf.framework/kperf";
  void *h = dlopen(path, RTLD_LAZY);
  if (!h) { fprintf(stderr, "dlopen: %s\n", dlerror()); return -1; }
  #define RESOLVE(sym) do { *(void**)(&sym) = dlsym(h, #sym); if (!(sym)) { fprintf(stderr, "dlsym(%s) failed\n", #sym); return -1; } } while (0)
  RESOLVE(kpc_set_counting);
  RESOLVE(kpc_set_thread_counting);
  RESOLVE(kpc_get_counter_count);
  RESOLVE(kpc_get_thread_counters);
  RESOLVE(kpc_force_all_ctrs_set);
  RESOLVE(kpc_force_all_ctrs_get);
  #undef RESOLVE
  return 0;
}

// ---- Stabilizers ----

// Prefer P-cores by raising QoS (no hard pinning to P/E on macOS).
static void prefer_interactive_qos(void) {
  // If available on your SDK:
  pthread_set_qos_class_self_np(QOS_CLASS_USER_INTERACTIVE, 0);
  // Portable fallback: raise priority slightly (optional); QoS is better when available.
}

// Keep the thread on a single core (soft affinity tag).
static void set_thread_affinity(int tag) {
  thread_affinity_policy_data_t pol = { tag };
  thread_policy_set(mach_thread_self(), THREAD_AFFINITY_POLICY,
                    (thread_policy_t)&pol, THREAD_AFFINITY_POLICY_COUNT);
}

// Read current thread's fixed counters into buf; returns count or 0 on error.
static uint32_t read_fixed(uint64_t *buf, uint32_t buf_len) {
  uint32_t need = kpc_get_counter_count(KPC_CLASS_FIXED_MASK);
  if (need == 0 || need > buf_len) return 0;
  if (kpc_get_thread_counters(0 /* this thread */, need, buf) != 0) return 0;
  return need;
}

#define MAXC 16
static uint64_t start_counters[MAXC];
static int kperf_ok = 0;

// Return instructions retired for running fn(arg) once, averaged over N iterations,
// repeated 'reps' times; returns the median per-call instruction count.
int
start_kperf(void) {
  if (load_kperf() != 0)
    return 0;

  // Acquire PMU if needed (often requires root)
  int blessed = 0;
  if (kpc_force_all_ctrs_get(&blessed) != 0)
    return 0;
  if (!blessed && kpc_force_all_ctrs_set(1) != 0)
    return 0;

  // Enable only FIXED counters (we'll read index 1 = instructions).
  if (kpc_set_counting(KPC_CLASS_FIXED_MASK) != 0)
    return 0;
  if (kpc_set_thread_counting(KPC_CLASS_FIXED_MASK) != 0)
    return 0;

  // Stabilize execution
  prefer_interactive_qos();
  set_thread_affinity(1);

  uint32_t n0 = read_fixed(start_counters, MAXC);
  if (n0 < 2) {
    fprintf(stderr, "Fixed counters < 2; instructions not available?\n");
    return 0;
  }
  kperf_ok = 1;
  return 1;
}

/* return number of instructions */
uint64_t
end_kperf(void)
{
  if (!kperf_ok)
    return 0;
  uint64_t end_counters[MAXC];
  uint32_t n0 = read_fixed(end_counters, MAXC);
  if (n0 < 2) {
    fprintf(stderr, "Fixed counters < 2; instructions not available?\n");
    return 0;
  }

  kpc_set_thread_counting(0);
  kpc_set_counting(0);
  kpc_force_all_ctrs_set(0);

  // On Apple silicon, fixed[1] is instructions retired.
  uint64_t instr = end_counters[1] - start_counters[1];
  return instr;
}
