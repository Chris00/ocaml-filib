/* File: filib_stubs.c

   Copyright (C) 2010

     Christophe Troestler <Christophe.Troestler@umons.ac.be>
     WWW: http://math.umons.ac.be/an/software/

   This library is free software; you can redistribute it and/or modify
   it under the terms of the GNU Lesser General Public License version 3 or
   later as published by the Free Software Foundation.  See the file
   LICENCE for more details.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
   LICENSE for more details. */


#include <interval/interval.hpp>

extern "C" {
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>
#include <caml/custom.h>
}

typedef filib::interval<double> interval;

#define EXPORT(f) extern "C" CAMLexport value filib_caml_ ## f

#define I_PTR(v) ((interval *) Data_custom_val(v))
#define I_VAL(v) (* (I_PTR(v)))

#define I_ALLOC() alloc_custom(&caml_filib_ops, sizeof(interval), 1, 100)

static void caml_filib_finalize(value v)
{
  delete(I_PTR(v));
}

static struct custom_operations caml_filib_ops = {
  (char *) "filib_t", /* identifier for serialization and deserialization */
  &caml_filib_finalize,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default };

EXPORT(init)(value vunit)
{
  filib::fp_traits<double>::setup();
  return Val_unit;
}

#define NEW_INTERVAL(name, f)                   \
  EXPORT(name)(value v)                         \
  {                                             \
    CAMLparam1(v);                              \
    CAMLlocal1(vi);                             \
    vi = I_ALLOC();                             \
    I_VAL(vi) = f;                              \
    CAMLreturn(vi);                             \
  }

NEW_INTERVAL(EMPTY,     interval::EMPTY())
NEW_INTERVAL(ENTIRE,    interval::ENTIRE())
NEW_INTERVAL(NEG_INFTY, interval::NEG_INFTY())
NEW_INTERVAL(POS_INFTY, interval::POS_INFTY())
NEW_INTERVAL(ZERO,      interval::ZERO())
NEW_INTERVAL(ONE,       interval::ONE())
NEW_INTERVAL(PI,        interval::PI())

NEW_INTERVAL(of_float,  interval(Double_val(v)))
NEW_INTERVAL(copy,      interval(I_VAL(v)))

EXPORT(interval)(value va, value vb)
{
  CAMLparam2(va, vb);
  CAMLlocal1(vi);
  vi = I_ALLOC();
  I_VAL(vi) = interval(Double_val(va), Double_val(vb));
  CAMLreturn(vi);
}

EXPORT(inf)(value vi)
{
  CAMLparam1(vi);
  CAMLreturn(caml_copy_double(filib::inf(I_VAL(vi))));
}

EXPORT(sup)(value vi)
{
  CAMLparam1(vi);
  CAMLreturn(caml_copy_double(filib::sup(I_VAL(vi))));
}


EXPORT(do_add)(value va, value vb)
{
  /* noalloc */
  I_VAL(va) += I_VAL(vb);
  return(Val_unit);
}

EXPORT(do_sub)(value va, value vb)
{
  /* noalloc */
  I_VAL(va) -= I_VAL(vb);
  return(Val_unit);
}

EXPORT(do_mul)(value va, value vb)
{
  /* noalloc */
  I_VAL(va) *= I_VAL(vb);
  return(Val_unit);
}

EXPORT(do_div)(value va, value vb)
{
  /* noalloc */
  I_VAL(va) /= I_VAL(vb);
  return(Val_unit);
}

EXPORT(do_add_float)(value va, value vb)
{
  /* noalloc */
  I_VAL(va) += Double_val(vb);
  return(Val_unit);
}

EXPORT(do_sub_float)(value va, value vb)
{
  /* noalloc */
  I_VAL(va) -= Double_val(vb);
  return(Val_unit);
}

EXPORT(do_mul_float)(value va, value vb)
{
  /* noalloc */
  I_VAL(va) *= Double_val(vb);
  return(Val_unit);
}

EXPORT(do_div_float)(value va, value vb)
{
  /* noalloc */
  I_VAL(va) /= Double_val(vb);
  return(Val_unit);
}

#define BOOL_OP1(f)                             \
  EXPORT(f)(value vi)                           \
  {                                             \
    /* noalloc */                               \
    return(Val_bool(f(I_VAL(vi))));             \
  }

BOOL_OP1(isPoint)
BOOL_OP1(isInfinite)
BOOL_OP1(isEmpty)

#define FLOAT_OP1(f)                            \
  EXPORT(f)(value va)                           \
  {                                             \
    CAMLparam1(va);                             \
    CAMLreturn(copy_double(f(I_VAL(va))));      \
  }
  
FLOAT_OP1(mid)
FLOAT_OP1(diam)
FLOAT_OP1(relDiam)
FLOAT_OP1(rad)
FLOAT_OP1(mig)
FLOAT_OP1(mag)

#define OP1(f)                                  \
  EXPORT(f)(value vi)                           \
  {                                             \
    CAMLparam1(vi);                             \
    CAMLlocal1(vo);                             \
    vo = I_ALLOC();                             \
    I_VAL(vo) = f(I_VAL(vi));                   \
    CAMLreturn(vo);                             \
  }
  
OP1(abs)
OP1(acos)
OP1(acosh)
OP1(acoth)
OP1(asin)
OP1(atan)
OP1(atanh)
OP1(cos)
OP1(cosh)
OP1(cot)
OP1(coth)
OP1(exp)
OP1(exp10)
OP1(exp2)
OP1(expm1)
OP1(log)
OP1(log10)
OP1(log1p)
OP1(log2)
OP1(sin)
OP1(sinh)
OP1(sqr)
OP1(sqrt)
OP1(tan)
OP1(tanh)

#define FLOAT_OP2(f)                                    \
  EXPORT(f)(value vi1, value vi2)                       \
  {                                                     \
    CAMLparam1(vi1, vi2);                               \
    CAMLreturn(copy_double(f(I_VAL(vi1), I_VAL(vi2)))); \
  }

#define OP2(f, ty1, ty2)                        \
  EXPORT(f)(value vi1, value vi2)               \
  {                                             \
    CAMLparam1(vi1, vi2);                       \
    CAMLlocal1(vo);                             \
    vo = I_ALLOC();                             \
    I_VAL(vo) = f(ty1(vi1), ty2(vi2));          \
    CAMLreturn(vo);                             \
  }

OP2(imin, I_VAL, I_VAL)
OP2(imax, I_VAL, I_VAL)
FLOAT_OP2(dist)
OP2(blow, I_VAL, Double_val)
OP2(intersect, I_VAL, I_VAL)
OP2(hull, I_VAL, I_VAL)
OP2(hull_float, Double_val, I_VAL)
OP2(hull_float2, Double_val, Double_val)

#define REL2(f, ty1, ty2)                       \
  EXPORT(f)(value vi1, value vi2)               \
  {                                             \
    /* noalloc */                               \
    return Val_bool(f(ty1(vi1), ty2(vi2)));     \
  }

REL2(disjoint, I_VAL, I_VAL)
REL2(in, Double_val, I_VAL)
REL2(interior, I_VAL, I_VAL)
REL2(proper_subset, I_VAL, I_VAL)
REL2(subset, I_VAL, I_VAL)
REL2(proper_superset, I_VAL, I_VAL)
REL2(superset, I_VAL, I_VAL)
