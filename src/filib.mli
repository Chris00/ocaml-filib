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

(** Binding to the Filib++ library.

    @version 0.3
 *)

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

val zero : unit -> rw t
(** [zero()] returns a new interval, initially equal to [[0., 0.]]. *)

val one : unit -> rw t
(** [one()] returns a new interval, initially equal to [[1., 1.]]. *)

val pi : ro t
(** [pi] an interval enclosing π. *)

val infinity : unit -> rw t
(** [infinity()] returns a new interval, initially equal to [[1.79769e+308, +∞]]. *)

val neg_infinity : unit -> rw t
(** [neg_infinity()] returns a new interval, initially equal to [[-∞,
    -1.79769e+308]]. *)

val of_float : float -> rw t
(** [of_float a] returns a new interval initialized to [[a,a]].
    Beware that the value of [a] may be different from the literal in
    your code.  For example, [of_float 0.1] will not contain the real
    number 0.1 because the literal [0.1] in your code will be rounded
    to the nearest float by the compiler.  To avoid such mistakes, it
    is strongly recommended to use the syntax extension. *)

val interval : float -> float -> rw t
(** [interval a b] returns a new interval initialized with [[a,b]].
    FIXME: what if a > b ? *)

val copy : 'a t -> rw t
(** [copy x] returns a copy the the interval [x]. *)

val inf : 'a t -> float
(** [inf x] lower bound of the interval [x].  Returns NaN if [x] is empty. *)
val sup : 'a t -> float
(** [sup x] upper bound of the interval [x].  Returns NaN if [x] is empty. *)

val to_string : 'a t -> string

val print : out_channel -> 'a t -> unit
val pretty_print : Format.formatter -> 'a t -> unit


(** {2 Access and Information} *)

external is_empty : 'a t -> bool = "filib_caml_isEmpty" "noalloc"
(** [is_empty x] tells whether the interval [x] is empty or not. *)

external is_point : 'a t -> bool = "filib_caml_isPoint" "noalloc"
(** [is_point i] returns true, iff [i] is a point interval. *)

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


(** {2 Arithmetic operations} *)

external add : 'a t -> 'a t -> rw t = "filib_caml_add"
external sub : 'a t -> 'a t -> rw t = "filib_caml_sub"
external mul : 'a t -> 'a t -> rw t = "filib_caml_mul"
external div : 'a t -> 'a t -> rw t = "filib_caml_div"


(** {2 Elementary functions} *)

external abs : 'a t -> rw t = "filib_caml_abs"
(** [abs i] returns the interval of all absolute values (moduli) of
    [i]: [abs i = [mig i, mag i]].  The following cases are considered:
    - [abs i = ∅] if [i = ∅];
    - [abs i = [mig i, infinity]] if [is_infinite i] and one bound is finite
    - [abs i = [M, infinity]] if both bounds are infinite. *)

external acos : 'a t -> rw t = "filib_caml_acos"
external acosh : 'a t -> rw t = "filib_caml_acosh"
external acoth : 'a t -> rw t = "filib_caml_acoth"
external asin : 'a t -> rw t = "filib_caml_asin"
external asinh : 'a t -> rw t = "filib_caml_asinh"
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
external power : 'a t -> int -> rw t = "filib_caml_power"
external pow : 'a t -> 'b t -> rw t = "filib_caml_pow"
external sin : 'a t -> rw t = "filib_caml_sin"
external sinh : 'a t -> rw t = "filib_caml_sinh"
external sqr : 'a t -> rw t = "filib_caml_sqr"
external sqrt : 'a t -> rw t = "filib_caml_sqrt"
external tan : 'a t -> rw t = "filib_caml_tan"
external tanh : 'a t -> rw t = "filib_caml_tanh"

(** {2 Set Theoretic functions} *)

external min : 'a t -> 'a t -> rw t = "filib_caml_imin"
(** [min a b] returns an enclosure of the interval of all minima of
    [a] and [b], i.e. [min a b = {min{x,y} | x ∈ a, y ∈ b }].  In
    particular [min a b = ∅] if [a = ∅] or [b = ∅]. *)

external max : 'a t -> 'a t -> rw t = "filib_caml_imax"
(** [max a b] returns an enclosure of the interval of all maxima of
    [a] and [b], i.e. [max a b = {max{x,y} | x ∈ a, y ∈ b }].  In
    particular [max a b = ∅] if [a = ∅] or [b = ∅]. *)

external dist : 'a t -> 'a t -> float = "filib_caml_dist"
(** [dist a b] returns an upper bound of the Hausdorff-distance of [a]
    and [b], i.e. [dist a b = max{ abs(inf(a) - inf(b)), abs(sup(a) -
    sup(b)) }].  [dist a b = NaN] when [a = ∅] or [b = ∅]. *)

external blow : 'a t -> float -> rw t = "filib_caml_blow"
(** [blow a] return the ε-inflation: [blow a = (1 + eps) · a - eps · a]. *)

external hull : 'a t -> 'a t -> rw t = "filib_caml_hull"
(** [hull a b] the interval hull of [a] and [b]. *)

external disjoint : 'a t -> 'a t -> bool = "filib_caml_disjoint" "noalloc"
(** [disjoint a b] returns [true], iff [a] and [b] are disjoint,
    i.e. [intersect a b = ∅]. *)

external belongs : float -> 'a t -> bool = "filib_caml_in" "noalloc"
(** [belongs x a] returns [true] iff [x ∈ a]. *)

external interior : 'a t -> 'a t -> bool = "filib_caml_interior" "noalloc"
(** [interior a b] returns [true], iff [a] is contained in the
    interior of [b]. *)

external proper_subset : 'a t -> 'a t -> bool = "filib_caml_proper_subset"
  "noalloc"
(** [proper_subset a b] returns [true], iff [a] is a proper subset of [b]. *)

external subset : 'a t -> 'a t -> bool = "filib_caml_subset" "noalloc"
(** [subset a b] returns [true], iff [a] is a subset of [b]. *)

external proper_superset : 'a t -> 'a t -> bool = "filib_caml_proper_superset"
  "noalloc"
(** [proper_superset a b] returns [true], iff [a] is a proper superset
    of [b]. *)

external superset : 'a t -> 'a t -> bool = "filib_caml_superset" "noalloc"
(** [superset a b] returns [true], iff [a] is a superset of [b]. *)


(** {2 Set relational functions} *)

external seq : 'a t -> 'b t -> bool = "filib_caml_seq"
(** [seq a b] returns [true], iff [a] and [b] are equal sets. *)
external sne : 'a t -> 'b t -> bool = "filib_caml_sne"
(** [sne a b] returns [true], iff [a] and [b] are not equal sets. *)
external sge : 'a t -> 'b t -> bool = "filib_caml_sge"
(** [sge a b] returns [true], iff the ≥ relation holds for the bounds:
    [sge a b = inf(a) >= inf(b) && sup(a) >= sup(b)].  It returns [true], if
    [a = ∅] and [b = ∅].  *)
external sgt : 'a t -> 'b t -> bool = "filib_caml_sgt"
(** [sgt a b] returns [true], iff the > relation holds for the bounds:
    [sgt a b = inf(a) > inf(b) && sup(a) > sup(b)].  It returns [false],
    if [a = ∅] and [b = ∅].  *)
external sle : 'a t -> 'b t -> bool = "filib_caml_sle"
(** [sle a b] returns [true], iff the ≤ relation holds for the bounds:
    [sle a b = inf(a) <= inf(b) && sup(a) <= sup(b)].  It returns [true],
    if [a = ∅] and [b = ∅].  *)
external slt : 'a t -> 'b t -> bool = "filib_caml_slt"
(** [slt a b] returns [true], iff the < relation holds for the bounds:
    [slt a b = inf(a) < inf(b) && sup(a) < sup(b)].  It return [false],
    if [a = ∅] and [b = ∅]. *)

(** {2 Certainly comparison operators} *)

external ceq : 'a t -> 'b t -> bool = "filib_caml_ceq"
(** [ceq a b] returns [true], iff the = relation holds for all
    individual points from [a] and [b], i.e. ∀t ∈ a, ∀x ∈ b : t = x.
    That implies that [a] and [b] are point intervals.  Return
    [false], if [a = ∅] or [b = ∅].  *)
external cne : 'a t -> 'b t -> bool = "filib_caml_cne"
external cge : 'a t -> 'b t -> bool = "filib_caml_cge"
external cgt : 'a t -> 'b t -> bool = "filib_caml_cgt"
external cle : 'a t -> 'b t -> bool = "filib_caml_cle"
external clt : 'a t -> 'b t -> bool = "filib_caml_clt"

(** {2 Possibly comparison operators} *)

external peq : 'a t -> 'b t -> bool = "filib_caml_peq"
(** [peq a b] returns [true], iff the = relation holds for any points
    from [a] and [b], i.e. ∃t ∈ a, ∃x ∈ b : t = x.  Return false, if
    [a = ∅] or [b = ∅].  *)
external pne : 'a t -> 'b t -> bool = "filib_caml_pne"
external pge : 'a t -> 'b t -> bool = "filib_caml_pge"
external pgt : 'a t -> 'b t -> bool = "filib_caml_pgt"
external ple : 'a t -> 'b t -> bool = "filib_caml_ple"
external plt : 'a t -> 'b t -> bool = "filib_caml_plt"


module Do :
sig

  external interval : rw t -> float -> float -> unit
    = "filib_caml_do_interval" "noalloc"

  external copy : rw t -> 'a t -> unit = "filib_caml_do_copy" "noalloc"
  (** [copy y x] performs [y <- x]. *)

  external neg : rw t -> 'a t -> unit = "filib_caml_do_neg" "noalloc"
  (** Unary negation. *)

  external add : rw t -> 'a t -> 'b t -> unit = "filib_caml_do_add" "noalloc"
  external sub : rw t -> 'a t -> 'b t -> unit = "filib_caml_do_sub" "noalloc"
  external mul : rw t -> 'a t -> 'b t -> unit = "filib_caml_do_mul" "noalloc"
  external div : rw t -> 'a t -> 'b t -> unit = "filib_caml_do_div" "noalloc"

  external add_to : rw t -> 'a t -> unit = "filib_caml_do_add_to" "noalloc"
  external sub_to : rw t -> 'a t -> unit = "filib_caml_do_sub_to" "noalloc"
  external mul_to : rw t -> 'a t -> unit = "filib_caml_do_mul_to" "noalloc"
  external div_to : rw t -> 'a t -> unit = "filib_caml_do_div_to" "noalloc"

  external add_float : rw t -> 'a t -> float -> unit
    = "filib_caml_do_add_float" "noalloc"
  external sub_float : rw t -> 'a t -> float -> unit
    = "filib_caml_do_sub_float" "noalloc"
  external float_sub : rw t -> float -> 'a t -> unit
    = "filib_caml_do_float_sub" "noalloc"
  external mul_float : rw t -> 'a t -> float -> unit
    = "filib_caml_do_mul_float" "noalloc"
  external div_float : rw t -> 'a t -> float -> unit
    = "filib_caml_do_div_float" "noalloc"
  external float_div : rw t -> float -> 'a t -> unit
    = "filib_caml_do_float_div" "noalloc"


  (** {2 Elementary functions, imperative version} *)

  external abs : rw t -> 'a t -> unit = "filib_caml_do_abs" "noalloc"
  (** [abs y x] puts the result of {!Filib.abs}[x] in [y].  This
      version is more efficient than {!Filib.abs} because no new
      variable is allocated. *)
  external acos : rw t -> 'a t -> unit = "filib_caml_do_acos" "noalloc"
  external acosh : rw t -> 'a t -> unit = "filib_caml_do_acosh" "noalloc"
  external acoth : rw t -> 'a t -> unit = "filib_caml_do_acoth" "noalloc"
  external asin : rw t -> 'a t -> unit = "filib_caml_do_asin" "noalloc"
  external asinh : rw t -> 'a t -> unit = "filib_caml_do_asinh" "noalloc"
  external atan : rw t -> 'a t -> unit = "filib_caml_do_atan" "noalloc"
  external atanh : rw t -> 'a t -> unit = "filib_caml_do_atanh" "noalloc"
  external cos : rw t -> 'a t -> unit = "filib_caml_do_cos" "noalloc"
  external cosh : rw t -> 'a t -> unit = "filib_caml_do_cosh" "noalloc"
  external cot : rw t -> 'a t -> unit = "filib_caml_do_cot" "noalloc"
  external coth : rw t -> 'a t -> unit = "filib_caml_do_coth" "noalloc"
  external exp : rw t -> 'a t -> unit = "filib_caml_do_exp" "noalloc"
  external exp10 : rw t -> 'a t -> unit = "filib_caml_do_exp10" "noalloc"
  external exp2 : rw t -> 'a t -> unit = "filib_caml_do_exp2" "noalloc"
  external expm1 : rw t -> 'a t -> unit = "filib_caml_do_expm1" "noalloc"
  external log : rw t -> 'a t -> unit = "filib_caml_do_log" "noalloc"
  external log10 : rw t -> 'a t -> unit = "filib_caml_do_log10" "noalloc"
  external log1p : rw t -> 'a t -> unit = "filib_caml_do_log1p" "noalloc"
  external log2 : rw t -> 'a t -> unit = "filib_caml_do_log2" "noalloc"
  external power : rw t -> 'a t -> int -> unit = "filib_caml_do_power"
  external pow : rw t -> 'a t -> 'b t -> unit = "filib_caml_do_pow"
  external sin : rw t -> 'a t -> unit = "filib_caml_do_sin" "noalloc"
  external sinh : rw t -> 'a t -> unit = "filib_caml_do_sinh" "noalloc"
  external sqr : rw t -> 'a t -> unit = "filib_caml_do_sqr" "noalloc"
  external sqrt : rw t -> 'a t -> unit = "filib_caml_do_sqrt" "noalloc"
  external tan : rw t -> 'a t -> unit = "filib_caml_do_tan" "noalloc"
  external tanh : rw t -> 'a t -> unit = "filib_caml_do_tanh" "noalloc"


  (** {2 Set Theoretic functions, imperative versions} *)

  external min : rw t -> 'a t -> 'a t -> unit = "filib_caml_do_imin" "noalloc"
  (** [min y x1 x2] performs [y <- min x1 x2] (see {!Filib.min}). *)
  external max : rw t -> 'a t -> 'a t -> unit = "filib_caml_do_imax" "noalloc"
  external blow : rw t -> 'a t -> float -> unit = "filib_caml_do_blow" "noalloc"
  external hull : rw t -> 'a t -> 'a t -> unit = "filib_caml_do_hull" "noalloc"
  external hull_float : rw t -> float -> 'a t -> unit = "filib_caml_do_hull_float"
     "noalloc"
end
