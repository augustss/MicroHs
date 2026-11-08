/*
 * The JavaScript FFI of the emscripten target: what eval.c uses of jsffi.c.
 * Included by eval.c near the top; jsffi.c itself is included further down,
 * since it needs the evaluator's internals.
 */
extern int mhs_js_keep_alive;   /* the number of live JavaScript callbacks */
/* JavaScript values referenced by JSVal handles are only released by the GC
 * finalizers, so a GC is forced after this many new handles even if the heap
 * is not full (a large heap could otherwise keep them alive for a long time). */
#define JSVAL_GC_LIMIT 100000
extern uvalue_t num_jsval_alloc;

/* Set when main has started (mhs_main), as opposed to a program without main
 * whose foreign exports are called by JavaScript. */
static int main_started;

/* Non-zero while a JavaScript callback runs Haskell (see run_callback).  Nothing
 * may block there: the calling thread's C stack has been discarded, so there is
 * nothing to return to.  throwTo therefore does not wait for delivery. */
extern int in_js_callback;
/* The slice of a callback: it is not preempted, see run_callback(). */
#define CALLBACK_SLICE 1000000000

/* A combinator program supplied at run time (mhs_run_web), or 0. */
static const uint8_t *run_comb;
static int run_comblen;

static void js_init(void);
static int js_enter_main(int argc, char **argv);
static int js_suspend(void);
static void report_callback_exn(NODEPTR exn);
static NODEPTR js_await(NODEPTR kind, NODEPTR prom);
static void js_ffe_eval(void);
struct mthread;
static void js_throwto(struct mthread *mt, NODEPTR exn);
static void js_yield(void);
static void js_main_done(void);
