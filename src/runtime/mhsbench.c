#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

const uint8_t *combexpr = NULL;
const int combexprlen = 0;

#include "eval.c"

const struct ffi_entry *xffi_table = 0;
struct ffe_entry *xffe_table = 0;

static volatile size_t bench_sink = 0;

enum bench_mode {
  BENCH_WHNF,
  BENCH_MAIN,
};

static uint64_t
monotonic_ns(void)
{
  struct timespec ts;
  if (clock_gettime(CLOCK_MONOTONIC, &ts) != 0)
    ERR("clock_gettime failed");
  return (uint64_t)ts.tv_sec * 1000000000ull + (uint64_t)ts.tv_nsec;
}

static uint8_t *
read_file(const char *path, size_t *len)
{
  FILE *f = fopen(path, "rb");
  uint8_t *bytes;
  long size;

  if (!f) {
    fprintf(stderr, "%s: cannot open\n", path);
    exit(1);
  }
  if (fseek(f, 0, SEEK_END) != 0) {
    fprintf(stderr, "%s: seek failed\n", path);
    exit(1);
  }
  size = ftell(f);
  if (size < 0) {
    fprintf(stderr, "%s: tell failed\n", path);
    exit(1);
  }
  rewind(f);

  bytes = malloc((size_t)size);
  if (!bytes) {
    fprintf(stderr, "out of memory\n");
    exit(1);
  }
  if (fread(bytes, 1, (size_t)size, f) != (size_t)size) {
    fprintf(stderr, "%s: read failed\n", path);
    exit(1);
  }
  fclose(f);
  *len = (size_t)size;
  return bytes;
}

static void
init_runtime(void)
{
  heap_size = HEAP_CELLS;
  stack_size = STACK_SIZE;
  init_nodes();
  stack = mmalloc(sizeof(NODEPTR) * stack_size);
  CLEARSTK();
  init_stableptr();
#if WANT_ARGS
  {
    NODEPTR args = mkCons(mkStringC("mhsbench"), mkNil());
    argarray = arr_alloc(1, args);
    argarray->permanent = true;
  }
#endif
}

static void
start_whnf(NODEPTR root)
{
  struct mthread *mt;

  /* Like start_exec(), but evaluate the benchmark root itself instead of
   * applying the runtime World argument used by compiled main programs. */
  mt = new_thread(root);
  mt->mt_id = MAIN_THREAD;
  main_thread = mt;

  switch(setjmp(sched)) {
  case mt_main:
    break;
  case mt_resched:
    COUNT(num_resched);
    break;
  case mt_raise:
    if (in_raise) {
      ERR("FATAL: exception while trying to die");
      EXIT(1);
    }
    mt = remove_q_head(&runq);
    if (mt->mt_id == MAIN_THREAD) {
      die_exn(the_exn);
    } else {
      mt->mt_state = ts_died;
      mt->mt_root = NIL;
    }
  }
  for(;;) {
    if (!runq.mq_head)
      pause_exec();
    mt = runq.mq_head;
    if (!mt)
      ERR("no threads");

    glob_slice = mt->mt_slice + slice;
    num_reductions += glob_slice-1;
    (void)evali(mt->mt_root);
    num_reductions -= glob_slice;
    (void)remove_q_head(&runq);

    mt->mt_state = ts_finished;
    mt->mt_root = NIL;

    if (mt->mt_id == MAIN_THREAD) {
      main_thread = 0;
      return;
    }
  }
}

static void
bench_once(const uint8_t *input, size_t len, enum bench_mode mode)
{
  BFILE *in = openb_rd_mem(input, len);
  NODEPTR prog = parse_top(in, 0);
  BFILE *out;
  uint8_t *out_bytes;
  size_t out_len;

  closeb(in);
  CLEARSTK();
  if (mode == BENCH_MAIN)
    start_exec(prog);
  else
    start_whnf(prog);

  out = openb_wr_mem();
  printb(out, prog, false);
  putb('\n', out);
  get_mem(out, &out_bytes, &out_len);
  bench_sink += out_len;
  if (out_len)
    bench_sink += out_bytes[0];
  closeb(out);
}

static void
usage(void)
{
  fprintf(stderr, "usage: mhsbench [--mode whnf|main] --iters N FILE\n");
}

static const char *
mode_name(enum bench_mode mode)
{
  return mode == BENCH_MAIN ? "main" : "whnf";
}

static int
parse_mode(const char *text, enum bench_mode *mode)
{
  if (strcmp(text, "whnf") == 0) {
    *mode = BENCH_WHNF;
    return 1;
  }
  if (strcmp(text, "main") == 0) {
    *mode = BENCH_MAIN;
    return 1;
  }
  return 0;
}

int
main(int argc, char **argv)
{
  int iters = 0;
  const char *path = NULL;
  enum bench_mode mode = BENCH_WHNF;
  uint8_t *input;
  size_t len;
  uint64_t start;
  uint64_t elapsed;

  for (int i = 1; i < argc; i++) {
    if (strcmp(argv[i], "--iters") == 0 && i + 1 < argc) {
      iters = atoi(argv[++i]);
    } else if (strcmp(argv[i], "--mode") == 0 && i + 1 < argc) {
      if (!parse_mode(argv[++i], &mode)) {
        usage();
        return 2;
      }
    } else if (!path) {
      path = argv[i];
    } else {
      usage();
      return 2;
    }
  }
  if (iters <= 0 || !path) {
    usage();
    return 2;
  }

  input = read_file(path, &len);
  init_runtime();

  start = monotonic_ns();
  for (int i = 0; i < iters; i++)
    bench_once(input, len, mode);
  elapsed = monotonic_ns() - start;

  printf("iters: %d\n", iters);
  printf("c_bench_mode: %s\n", mode_name(mode));
  printf("c_parse_eval_serialize_total_ms: %.3f\n", (double)elapsed / 1000000.0);
  printf("c_parse_eval_serialize_ns_per_iter: %.1f\n", (double)elapsed / (double)iters);
  printf("c_bench_sink: %zu\n", (size_t)bench_sink);
  free(input);
  return 0;
}
