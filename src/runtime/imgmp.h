#ifndef IMGMP_H
#define IMGMP_H

#include <stddef.h>
#include <stdlib.h>
#include <limits.h>

#define mpz_t __imath_mpz_t
#include "imath.h"
#undef mpz_t

typedef __imath_mpz_t mpz_t[1];
typedef __imath_mpz_t* mpz_ptr;
typedef const __imath_mpz_t* mpz_srcptr;
typedef unsigned long int mp_bitcnt_t;

static inline void mpz_init(mpz_ptr x) {
    mp_int_init(x);
}

static inline void mpz_clear(mpz_ptr x) {
    mp_int_clear(x);
}

static inline int mpz_set_str(mpz_ptr rop, const char *str, int base) {
    while (*str == ' ' || *str == '\t' || *str == '\n' || *str == '\r') {
        str++;
    }
    int is_neg = 0;
    if (*str == '-') {
        is_neg = 1;
        str++;
    } else if (*str == '+') {
        str++;
    }
    
    // GMP base 0 automatic prefix detection detection
    if (base == 0) {
        if (*str == '0') {
            if (str[1] == 'x' || str[1] == 'X') { base = 16; str += 2; }
            else if (str[1] == 'b' || str[1] == 'B') { base = 2; str += 2; }
            else { base = 8; }
        } else {
            base = 10;
        }
    } else if (base == 16) {
        if (str[0] == '0' && (str[1] == 'x' || str[1] == 'X')) str += 2;
    } else if (base == 2) {
        if (str[0] == '0' && (str[1] == 'b' || str[1] == 'B')) str += 2;
    }
    
    if (mp_int_read_string(rop, base, str) == 0) {
        if (is_neg) mp_int_neg(rop, rop);
        return 0;
    }
    return -1;
}

static inline char *mpz_get_str(char *str, int base, mpz_srcptr op) {
    int actual_base = (base < 0) ? -base : (base == 0 ? 10 : base);
    int len = mp_int_string_len((mpz_ptr)op, actual_base);
    
    if (str == NULL) {
        str = (char *)malloc(len);
    }
    
    mp_int_to_string((mpz_ptr)op, actual_base, str, len);
    
    if (base >= 0) {
        for (int i = 0; str[i]; i++) {
            if (str[i] >= 'A' && str[i] <= 'Z') str[i] += 32;
        }
    }
    return str;
}

static inline unsigned long int mpz_get_ui(mpz_srcptr x) {
    mp_usmall val = 0;
    __imath_mpz_t local = *x;   
    local.sign = MP_ZPOS;       
    
    int max_digits = sizeof(unsigned long) / sizeof(mp_digit);
    if (local.used > max_digits) {
        local.used = max_digits; 
    }
    (void)mp_int_to_uint(&local, &val);
    return val;
}

static inline int mpz_sgn(mpz_srcptr x) {
    return mp_int_compare_zero((mpz_ptr)x);
}

static inline void mpz_init_set(mpz_ptr rop, mpz_srcptr op) {
    mp_int_init_copy(rop, (mpz_ptr)op);
}

static inline void mpz_init_set_si(mpz_ptr rop, signed long int op) {
    mp_int_init_value(rop, op);
}

static inline void mpz_init_set_ui(mpz_ptr rop, unsigned long int op) {
    mp_int_init_uvalue(rop, op);
}

static inline void mpz_add_ui(mpz_ptr rop, mpz_srcptr op1, unsigned long int op2) {
    // Intercept values exceeding signed boundaries to prevent negative interpretation bugs
    if (op2 <= (unsigned long)LONG_MAX) {
        mp_int_add_value((mpz_ptr)op1, (mp_small)op2, rop);
    } else {
        __imath_mpz_t tmp;
        mp_int_init_uvalue(&tmp, op2);
        mp_int_add((mpz_ptr)op1, &tmp, rop);
        mp_int_clear(&tmp);
    }
}

static inline void mpz_neg(mpz_ptr rop, mpz_srcptr op) {
    mp_int_neg((mpz_ptr)op, rop);
}

static inline void mpz_abs(mpz_ptr rop, mpz_srcptr op) {
    mp_int_abs((mpz_ptr)op, rop);
}

static inline void mpz_add(mpz_ptr rop, mpz_srcptr op1, mpz_srcptr op2) {
    mp_int_add((mpz_ptr)op1, (mpz_ptr)op2, rop);
}

static inline void mpz_sub(mpz_ptr rop, mpz_srcptr op1, mpz_srcptr op2) {
    mp_int_sub((mpz_ptr)op1, (mpz_ptr)op2, rop);
}

static inline void mpz_mul(mpz_ptr rop, mpz_srcptr op1, mpz_srcptr op2) {
    mp_int_mul((mpz_ptr)op1, (mpz_ptr)op2, rop);
}

static inline void mpz_mul_2exp(mpz_ptr rop, mpz_srcptr op, mp_bitcnt_t bitcnt) {
    mp_int_mul_pow2((mpz_ptr)op, bitcnt, rop);
}

static inline void mpz_tdiv_q_2exp(mpz_ptr q, mpz_srcptr n, mp_bitcnt_t bitcnt) {
    mp_int_div_pow2((mpz_ptr)n, bitcnt, q, NULL);
}

static inline void mpz_fdiv_q_2exp(mpz_ptr q, mpz_srcptr n, mp_bitcnt_t bitcnt) {
    __imath_mpz_t rem;
    mp_int_init(&rem);
    mp_int_div_pow2((mpz_ptr)n, bitcnt, q, &rem);
    if (mp_int_compare_zero((mpz_ptr)n) < 0 && mp_int_compare_zero(&rem) != 0) {
        mp_int_sub_value(q, 1, q);
    }
    mp_int_clear(&rem);
}

static inline void mpz_tdiv_qr(mpz_ptr q, mpz_ptr r, mpz_srcptr n, mpz_srcptr d) {
    mp_int_div((mpz_ptr)n, (mpz_ptr)d, q, r);
}

static inline int mpz_cmp(mpz_srcptr op1, mpz_srcptr op2) {
    return mp_int_compare((mpz_ptr)op1, (mpz_ptr)op2);
}

static inline double mpz_get_d(mpz_srcptr x) {
    int len = mp_int_string_len((mpz_ptr)x, 10);
    char *buf = (char *)malloc(len);
    mp_int_to_string((mpz_ptr)x, 10, buf, len);
    double d = strtod(buf, NULL);
    free(buf);
    return d;
}

static inline size_t mpz_sizeinbase(mpz_srcptr x, int base) {
    if (mp_int_compare_zero((mpz_ptr)x) == 0) return 1;
    int len = mp_int_string_len((mpz_ptr)x, base) - 1;
    if (mp_int_compare_zero((mpz_ptr)x) < 0) len--;
    return len;
}

static inline int mpz_tstbit(mpz_srcptr x, mp_bitcnt_t bitindex) {
    if (mp_int_compare_zero((mpz_ptr)x) >= 0) {
        __imath_mpz_t tmp;
        mp_int_init(&tmp);
        mp_int_div_pow2((mpz_ptr)x, bitindex, &tmp, NULL);
        int bit = mp_int_is_odd(&tmp);
        mp_int_clear(&tmp);
        return bit;
    } else {
        // Two's complement bit transformation rule for negative inputs
        __imath_mpz_t abs_m1;
        mp_int_init(&abs_m1);
        mp_int_abs((mpz_ptr)x, &abs_m1);
        mp_int_sub_value(&abs_m1, 1, &abs_m1);
        mp_int_div_pow2(&abs_m1, bitindex, &abs_m1, NULL);
        int bit = mp_int_is_odd(&abs_m1);
        mp_int_clear(&abs_m1);
        return !bit;
    }
}

static inline void mpz_and(mpz_ptr rop, mpz_srcptr op1, mpz_srcptr op2) {
    int s1 = mp_int_compare_zero((mpz_ptr)op1);
    int s2 = mp_int_compare_zero((mpz_ptr)op2);
    
    if (s1 >= 0 && s2 >= 0) {
        __imath_mpz_t t1, t2, res, bit_val;
        mp_int_init_copy(&t1, (mpz_ptr)op1); mp_int_init_copy(&t2, (mpz_ptr)op2);
        mp_int_init(&res); mp_int_init(&bit_val); mp_int_set_value(&bit_val, 1);
        while (mp_int_compare_zero(&t1) != 0 && mp_int_compare_zero(&t2) != 0) {
            if (mp_int_is_odd(&t1) && mp_int_is_odd(&t2)) mp_int_add(&res, &bit_val, &res);
            mp_int_mul_pow2(&bit_val, 1, &bit_val);
            mp_int_div_pow2(&t1, 1, &t1, NULL); mp_int_div_pow2(&t2, 1, &t2, NULL);
        }
        mp_int_copy(&res, rop);
        mp_int_clear(&t1); mp_int_clear(&t2); mp_int_clear(&res); mp_int_clear(&bit_val);
    } else if (s1 < 0 && s2 < 0) {
        // A & B = ~(~A | ~B)
        __imath_mpz_t not1, not2, or_res, bit_val;
        mp_int_init(&not1); mp_int_init(&not2); mp_int_init(&or_res); mp_int_init(&bit_val);
        mp_int_neg((mpz_ptr)op1, &not1); mp_int_sub_value(&not1, 1, &not1);
        mp_int_neg((mpz_ptr)op2, &not2); mp_int_sub_value(&not2, 1, &not2);
        mp_int_set_value(&bit_val, 1);
        while (mp_int_compare_zero(&not1) != 0 || mp_int_compare_zero(&not2) != 0) {
            if (mp_int_is_odd(&not1) || mp_int_is_odd(&not2)) mp_int_add(&or_res, &bit_val, &or_res);
            mp_int_mul_pow2(&bit_val, 1, &bit_val);
            mp_int_div_pow2(&not1, 1, &not1, NULL); mp_int_div_pow2(&not2, 1, &not2, NULL);
        }
        mp_int_neg(&or_res, rop); mp_int_sub_value(rop, 1, rop);
        mp_int_clear(&not1); mp_int_clear(&not2); mp_int_clear(&or_res); mp_int_clear(&bit_val);
    } else {
        // Mixed: pos_op & ~not_neg
        __imath_mpz_t pos_op, not_neg, res, bit_val;
        mp_int_init_copy(&pos_op, (mpz_ptr)(s1 >= 0 ? op1 : op2));
        mp_int_init(&not_neg); mp_int_neg((mpz_ptr)(s1 < 0 ? op1 : op2), &not_neg); mp_int_sub_value(&not_neg, 1, &not_neg);
        mp_int_init(&res); mp_int_init(&bit_val); mp_int_set_value(&bit_val, 1);
        while (mp_int_compare_zero(&pos_op) != 0) {
            if (mp_int_is_odd(&pos_op) && !mp_int_is_odd(&not_neg)) mp_int_add(&res, &bit_val, &res);
            mp_int_mul_pow2(&bit_val, 1, &bit_val);
            mp_int_div_pow2(&pos_op, 1, &pos_op, NULL); mp_int_div_pow2(&not_neg, 1, &not_neg, NULL);
        }
        mp_int_copy(&res, rop);
        mp_int_clear(&pos_op); mp_int_clear(&not_neg); mp_int_clear(&res); mp_int_clear(&bit_val);
    }
}

static inline void mpz_ior(mpz_ptr rop, mpz_srcptr op1, mpz_srcptr op2) {
    int s1 = mp_int_compare_zero((mpz_ptr)op1);
    int s2 = mp_int_compare_zero((mpz_ptr)op2);
    
    if (s1 >= 0 && s2 >= 0) {
        __imath_mpz_t t1, t2, res, bit_val;
        mp_int_init_copy(&t1, (mpz_ptr)op1); mp_int_init_copy(&t2, (mpz_ptr)op2);
        mp_int_init(&res); mp_int_init(&bit_val); mp_int_set_value(&bit_val, 1);
        while (mp_int_compare_zero(&t1) != 0 || mp_int_compare_zero(&t2) != 0) {
            if (mp_int_is_odd(&t1) || mp_int_is_odd(&t2)) mp_int_add(&res, &bit_val, &res);
            mp_int_mul_pow2(&bit_val, 1, &bit_val);
            mp_int_div_pow2(&t1, 1, &t1, NULL); mp_int_div_pow2(&t2, 1, &t2, NULL);
        }
        mp_int_copy(&res, rop);
        mp_int_clear(&t1); mp_int_clear(&t2); mp_int_clear(&res); mp_int_clear(&bit_val);
    } else if (s1 < 0 && s2 < 0) {
        // A | B = ~(~A & ~B)
        __imath_mpz_t not1, not2, and_res, bit_val;
        mp_int_init(&not1); mp_int_init(&not2); mp_int_init(&and_res); mp_int_init(&bit_val);
        mp_int_neg((mpz_ptr)op1, &not1); mp_int_sub_value(&not1, 1, &not1);
        mp_int_neg((mpz_ptr)op2, &not2); mp_int_sub_value(&not2, 1, &not2);
        mp_int_set_value(&bit_val, 1);
        while (mp_int_compare_zero(&not1) != 0 && mp_int_compare_zero(&not2) != 0) {
            if (mp_int_is_odd(&not1) && mp_int_is_odd(&not2)) mp_int_add(&and_res, &bit_val, &and_res);
            mp_int_mul_pow2(&bit_val, 1, &bit_val);
            mp_int_div_pow2(&not1, 1, &not1, NULL); mp_int_div_pow2(&not2, 1, &not2, NULL);
        }
        mp_int_neg(&and_res, rop); mp_int_sub_value(rop, 1, rop);
        mp_int_clear(&not1); mp_int_clear(&not2); mp_int_clear(&and_res); mp_int_clear(&bit_val);
    } else {
        // Mixed: - ( (~B) & ~A ) - 1
        __imath_mpz_t pos_op, not_neg, and_not, bit_val;
        mp_int_init_copy(&pos_op, (mpz_ptr)(s1 >= 0 ? op1 : op2));
        mp_int_init(&not_neg); mp_int_neg((mpz_ptr)(s1 < 0 ? op1 : op2), &not_neg); mp_int_sub_value(&not_neg, 1, &not_neg);
        mp_int_init(&and_not); mp_int_init(&bit_val); mp_int_set_value(&bit_val, 1);
        while (mp_int_compare_zero(&not_neg) != 0) {
            if (mp_int_is_odd(&not_neg) && !mp_int_is_odd(&pos_op)) mp_int_add(&and_not, &bit_val, &and_not);
            mp_int_mul_pow2(&bit_val, 1, &bit_val);
            mp_int_div_pow2(&not_neg, 1, &not_neg, NULL); mp_int_div_pow2(&pos_op, 1, &pos_op, NULL);
        }
        mp_int_neg(&and_not, rop); mp_int_sub_value(rop, 1, rop);
        mp_int_clear(&pos_op); mp_int_clear(&not_neg); mp_int_clear(&and_not); mp_int_clear(&bit_val);
    }
}

static inline void mpz_xor(mpz_ptr rop, mpz_srcptr op1, mpz_srcptr op2) {
    int s1 = mp_int_compare_zero((mpz_ptr)op1);
    int s2 = mp_int_compare_zero((mpz_ptr)op2);
    
    if ((s1 >= 0 && s2 >= 0) || (s1 < 0 && s2 < 0)) {
        __imath_mpz_t t1, t2, res, bit_val;
        mp_int_init(&res); mp_int_init(&bit_val); mp_int_set_value(&bit_val, 1);
        if (s1 >= 0) {
            mp_int_init_copy(&t1, (mpz_ptr)op1); mp_int_init_copy(&t2, (mpz_ptr)op2);
        } else {
            mp_int_init(&t1); mp_int_neg((mpz_ptr)op1, &t1); mp_int_sub_value(&t1, 1, &t1);
            mp_int_init(&t2); mp_int_neg((mpz_ptr)op2, &t2); mp_int_sub_value(&t2, 1, &t2);
        }
        while (mp_int_compare_zero(&t1) != 0 || mp_int_compare_zero(&t2) != 0) {
            if (mp_int_is_odd(&t1) ^ mp_int_is_odd(&t2)) mp_int_add(&res, &bit_val, &res);
            mp_int_mul_pow2(&bit_val, 1, &bit_val);
            mp_int_div_pow2(&t1, 1, &t1, NULL); mp_int_div_pow2(&t2, 1, &t2, NULL);
        }
        mp_int_copy(&res, rop);
        mp_int_clear(&t1); mp_int_clear(&t2); mp_int_clear(&res); mp_int_clear(&bit_val);
    } else {
        // Mixed: - (pos_op ^ (~neg_op)) - 1
        __imath_mpz_t pos_op, not_neg, xor_res, bit_val;
        mp_int_init_copy(&pos_op, (mpz_ptr)(s1 >= 0 ? op1 : op2));
        mp_int_init(&not_neg); mp_int_neg((mpz_ptr)(s1 < 0 ? op1 : op2), &not_neg); mp_int_sub_value(&not_neg, 1, &not_neg);
        mp_int_init(&xor_res); mp_int_init(&bit_val); mp_int_set_value(&bit_val, 1);
        while (mp_int_compare_zero(&pos_op) != 0 || mp_int_compare_zero(&not_neg) != 0) {
            if (mp_int_is_odd(&pos_op) ^ mp_int_is_odd(&not_neg)) mp_int_add(&xor_res, &bit_val, &xor_res);
            mp_int_mul_pow2(&bit_val, 1, &bit_val);
            mp_int_div_pow2(&pos_op, 1, &pos_op, NULL); mp_int_div_pow2(&not_neg, 1, &not_neg, NULL);
        }
        mp_int_neg(&xor_res, rop); mp_int_sub_value(rop, 1, rop);
        mp_int_clear(&pos_op); mp_int_clear(&not_neg); mp_int_clear(&xor_res); mp_int_clear(&bit_val);
    }
}

static inline mp_bitcnt_t mpz_popcount(mpz_srcptr x) {
    if (mp_int_compare_zero((mpz_ptr)x) < 0) {
        return ~(mp_bitcnt_t)0;
    }

    __imath_mpz_t t;
    mp_int_init_copy(&t, (mpz_ptr)x);
    mp_bitcnt_t count = 0;
    while (mp_int_compare_zero(&t) != 0) {
        if (mp_int_is_odd(&t)) count++;
        mp_int_div_pow2(&t, 1, &t, NULL);
    }
    mp_int_clear(&t);
    return count;
}

static inline int mpz_init_set_str(mpz_ptr rop, const char *str, int base) {
    mp_int_init(rop);
    if (mpz_set_str(rop, str, base) == 0) {
        return 0;
    }
    return -1;
}

#endif /* IMGMP_H */
