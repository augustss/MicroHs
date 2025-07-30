#ifndef MHSEVAL_H
#define MHSEVAL_H

#include <stddef.h>
#include <stdint.h>

// Opaque handle for MicroHs evaluation context
typedef struct MhsContext* MhsContextPtr;

// Initialize the MicroHs evaluator
MhsContextPtr mhs_init_context(void);

// Cleanup the context
void mhs_free_context(MhsContextPtr ctx);

// Evaluate a combinator expression from a string
// Returns 0 on success, error code on failure
// The result is returned as a string in 'result' and its length in 'result_len'
int mhs_eval_string(MhsContextPtr ctx, const char* expr, size_t len, char** result, size_t* result_len);

// run a combinator expression from a string without returning a result string.
// This is typically used for running programs with side effects where main has type main :: IO ()
// Returns 0 on success, error code on failure
int mhs_run_string(MhsContextPtr ctx, const char* expr, size_t len);

// Free result string allocated by eval functions
void mhs_free_result(char* result);

// Get last error message
const char* mhs_get_error(MhsContextPtr ctx);

#endif