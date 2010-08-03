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

typedef filib::interval<double,filib::native_switched,filib::i_mode_extended>
interval;

#define EXPORT(f) extern "C" CAMLexport value filib_caml_ ## f

#define I_PTR(v) ((interval *) Data_custom_val(v))
#define I_VAL(v) (* (I_PTR(v)))

#define I_SET(v, e)                                                    \
  v = alloc_custom(&caml_filib_ops, sizeof(interval), 1, 300000);      \
  I_VAL(v) = e

static void caml_filib_finalize(value v)
{
  // delete(I_PTR(v)); // the custom block holds the object.
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
    I_SET(vi, f);                               \
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
  I_SET(vi, interval(Double_val(va), Double_val(vb)));
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

EXPORT(do_float_sub)(value va, value vb)
{
  /* noalloc */
  I_VAL(va) = Double_val(vb) - I_VAL(va);
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

EXPORT(do_float_div)(value va, value vb)
{
  /* noalloc */
  I_VAL(va) = Double_val(vb) / I_VAL(va);
  return(Val_unit);
}

#define ARITH(name, op)                         \
  EXPORT(name)(value vi1, value vi2)            \
  {                                             \
    CAMLparam2(vi1, vi2);                       \
    CAMLlocal1(vo);                             \
    I_SET(vo, I_VAL(vi1) op I_VAL(vi2));        \
    CAMLreturn(vo);                             \
  }

ARITH(add, + )
ARITH(sub, - )
ARITH(mul, * )
ARITH(div, / )

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
    I_SET(vo, f(I_VAL(vi)));                    \
    CAMLreturn(vo);                             \
  }                                             \
  EXPORT(do_ ## f)(value vo, value vi)          \
  {                                             \
    /* noalloc */                               \
    I_VAL(vo) = f(I_VAL(vi));                   \
    return(Val_unit);                           \
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
    CAMLparam2(vi1, vi2);                               \
    CAMLreturn(copy_double(f(I_VAL(vi1), I_VAL(vi2)))); \
  }

#define OP2(name, f, ty1, ty2)                        \
  EXPORT(name)(value vi1, value vi2)                  \
  {                                                   \
    CAMLparam2(vi1, vi2);                             \
    CAMLlocal1(vo);                                   \
    I_SET(vo, filib::f(ty1(vi1), ty2(vi2)));          \
    CAMLreturn(vo);                                   \
  }                                                   \
  EXPORT(do_ ## name)(value vo, value vi1, value vi2) \
  {                                                   \
    /* noalloc */                                     \
    I_VAL(vo) = filib::f(ty1(vi1), ty2(vi2));         \
    return Val_unit;                                  \
  }
  

OP2(imin, imin, I_VAL, I_VAL)
OP2(imax, imax, I_VAL, I_VAL)
FLOAT_OP2(dist)
OP2(blow, blow, I_VAL, Double_val)
OP2(intersect, intersect, I_VAL, I_VAL)
OP2(hull,        hull, I_VAL, I_VAL)
OP2(hull_float,  hull, Double_val, I_VAL)
//OP2(hull_float2, hull, Double_val, Double_val)

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

REL2(seq, I_VAL, I_VAL)
REL2(sne, I_VAL, I_VAL)
REL2(sge, I_VAL, I_VAL)
REL2(sgt, I_VAL, I_VAL)
REL2(sle, I_VAL, I_VAL)
REL2(slt, I_VAL, I_VAL)

REL2(ceq, I_VAL, I_VAL)
REL2(cne, I_VAL, I_VAL)
REL2(cge, I_VAL, I_VAL)
REL2(cgt, I_VAL, I_VAL)
REL2(cle, I_VAL, I_VAL)
REL2(clt, I_VAL, I_VAL)

REL2(peq, I_VAL, I_VAL)
REL2(pne, I_VAL, I_VAL)
REL2(pge, I_VAL, I_VAL)
REL2(pgt, I_VAL, I_VAL)
REL2(ple, I_VAL, I_VAL)
REL2(plt, I_VAL, I_VAL)
