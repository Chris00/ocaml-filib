(* File: filib.mli

   Copyright (C) 2010

     Christophe Troestler <Christophe.Troestler@umons.ac.be>
     WWW: http://math.umons.ac.be/an/software/

   This library is free software; you can redistribute it and/or modify
   it under the terms of the GNU Lesser General Public License version 3 or
   later as published by the Free Software Foundation, with the special
   exception on linking described in the file LICENSE.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
   LICENSE for more details. *)

type ro
type rw = private ro

type +'a t
(** Mutable interval *)

type ro_t = ro t  (** Handy shortcut for type coercion. *)
type rw_t = rw t  (** Handy shortcut for type coercion. *)


val empty : unit -> rw t
(** [empty()] returns a new interval, initially empty (both bounds are
    set to NaN). *)

val entire : unit -> rw t
(** [entire()] returns a new interval, initially equal to [[-∞, ∞]]. *)

val of_float : float -> rw t
(** [of_float a] returns a new interval initialized to [[a,a]].
    Beware that the value of [a] may be different from the literal in
    your code.  For example, [of_float 0.1] will not contain the real
    0.1 because [0.1] in your code will be rounded to the nearest
    float.  To avoid such mistakes, use the syntax extension. *)

val interval : float -> float -> rw t
(** [interval a b] returns a new interval initialized with [[a,b]].
    FIXME: what if a > b ? *)

val copy : 'a t -> rw t
(** [copy x] returns a copy the the interval [x]. *)

val inf : 'a t -> float
(** [inf x] lower bound of the interval [x].  Returns NaN if [x] is empty. *)
val sup : 'a t -> float
(** [sup x] upper bound of the interval [x].  Returns NaN if [x] is empty. *)

val is_empty : 'a t -> bool
(** [is_empty x] tells whether the interval [x] is empty or not. *)

val to_string : 'a t -> string

val print : Format.formatter -> 'a t -> unit

external is_point : 'a t -> bool = "filib_caml_isPoint" "noalloc"
(** [is_point i] returns true, iff [i] is a point interval. *)

external is_empty : 'a t -> bool = "filib_caml_isEmpty" "noalloc"

external is_infinite : 'a t -> bool = "filib_caml_isInfinite" "noalloc"

external mid : 'a t -> float = "filib_caml_mid"
(** [mif i] returns an approximation of the midpoint of [i], that is
    contained in [i].  The following cases are distinguished:
    {[
    mid i = NaN   if i = ∅
            0.    if i = ]-∞, ∞[
            infinity     if i = [a, +∞[
            neg_infinity if i = ]-∞, a]
    ]} *)

val diam : 'a t -> float
(** Returns the diameter or width of the interval (upwardly rounded). *)

val rel_diam : 'a t -> float
(** [rel_diam i] returns an upper bound for the relative diameter of
    [i]: [rel_diam i = diam i] if [mig i] is less than the smallest
    positive normalized floating-point number, [rel_diam i = diam i /.
    mig i] otherwise.  The following cases are also distinguished:
    {[
    rel_diam i = NaN if i = ∅
                 infinity   if is_infinite i
    ]} *)

val rad : 'a t -> float
(** [rad i] returns the radius of [i] (upwardly rounded).  The
    following cases are considered:
    {[
    rad i = NaN   if i = ∅
            infinity     if is_infinite i
    ]}  *)

external mig : 'a t -> float = "filib_caml_mig"
(** [mig i] returns the mignitude, i.e. [mig i = min{abs(t) | t ∈ i}].
    If [i = ∅], [mig i = NaN]. *)
external mag : 'a t -> float = "filib_caml_mag"
(** [mag i] returns the magnitude, the absolute value of [i]; also
    [mag i = max{abs(t) | t ∈ i }].  If [i = ∅], then [mag i = NaN].
    If [is_infinite i], then [mag i = infinite]. *)

external abs : 'a t -> rw t = "filib_caml_abs"
(** [abs i] returns the interval of all absolute values (moduli) of
    [i]: [abs i = [mig i, mag i]].  The following cases are considered:
    - [abs i = ∅] if [i = ∅];
    - [abs i = [mig i, infinite]] if [is_infinite i] and one bound is finite
    - [abs i = [M, infinite]] if both bounds are infinite. *)

external acos : 'a t -> rw t = "filib_caml_acos"
external acosh : 'a t -> rw t = "filib_caml_acosh"
external acoth : 'a t -> rw t = "filib_caml_acoth"
external asin : 'a t -> rw t = "filib_caml_asin"
external atan : 'a t -> rw t = "filib_caml_atan"
external atanh : 'a t -> rw t = "filib_caml_atanh"
external cos : 'a t -> rw t = "filib_caml_cos"
external cosh : 'a t -> rw t = "filib_caml_cosh"
external cot : 'a t -> rw t = "filib_caml_cot"
external coth : 'a t -> rw t = "filib_caml_coth"
external exp : 'a t -> rw t = "filib_caml_exp"
external exp10 : 'a t -> rw t = "filib_caml_exp10"
external exp2 : 'a t -> rw t = "filib_caml_exp2"
external expm1 : 'a t -> rw t = "filib_caml_expm1"
external log : 'a t -> rw t = "filib_caml_log"
external log10 : 'a t -> rw t = "filib_caml_log10"
external log1p : 'a t -> rw t = "filib_caml_log1p"
external log2 : 'a t -> rw t = "filib_caml_log2"
external sin : 'a t -> rw t = "filib_caml_sin"
external sinh : 'a t -> rw t = "filib_caml_sinh"
external sqr : 'a t -> rw t = "filib_caml_sqr"
external sqrt : 'a t -> rw t = "filib_caml_sqrt"
external tan : 'a t -> rw t = "filib_caml_tan"
external tanh : 'a t -> rw t = "filib_caml_tanh"


module Do :
sig

  external add : rw t -> 'a t -> unit = "filib_caml_do_add" "noalloc"
  external sub : rw t -> 'a t -> unit = "filib_caml_do_sub" "noalloc"
  external mul : rw t -> 'a t -> unit = "filib_caml_do_mul" "noalloc"
  external div : rw t -> 'a t -> unit = "filib_caml_do_div" "noalloc"

  external add_float : rw t -> float -> unit
    = "filib_caml_do_add_float" "noalloc"
  external sub_float : rw t -> float -> unit
    = "filib_caml_do_sub_float" "noalloc"
  external mul_float : rw t -> float -> unit
    = "filib_caml_do_mul_float" "noalloc"
  external div_float : rw t -> float -> unit
    = "filib_caml_do_div_float" "noalloc"
end
