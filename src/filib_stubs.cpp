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
