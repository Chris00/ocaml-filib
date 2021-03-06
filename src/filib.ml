(* File: filib.ml

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

type ro_t = ro t
type rw_t = rw t

external setup : unit -> unit = "filib_caml_setup" "noalloc"

let () = setup()

external empty : unit -> rw t = "filib_caml_EMPTY"
external entire : unit -> rw t = "filib_caml_ENTIRE"
external neg_infinity : unit -> rw t = "filib_caml_NEG_INFTY"
external infinity : unit -> rw t = "filib_caml_POS_INFTY"
external zero : unit -> rw t = "filib_caml_ZERO"
external one : unit -> rw t = "filib_caml_ONE"
external make_pi : unit -> rw t = "filib_caml_PI"

(* It is better to have [pi] as a constant to use in formulae
   (especially convenient because the local open will mask a [pi] that
   the code would define). *)
let pi = (make_pi() :> ro t)

external of_float : float -> rw t = "filib_caml_of_float"
external interval : float -> float -> rw t = "filib_caml_interval"
external copy : 'a t -> rw t = "filib_caml_copy"

external inf : 'a t -> float = "filib_caml_inf"
external sup : 'a t -> float = "filib_caml_sup"

external is_empty : 'a t -> bool = "filib_caml_isEmpty" "noalloc"
external is_point : 'a t -> bool = "filib_caml_isPoint" "noalloc"
external is_infinite : 'a t -> bool = "filib_caml_isInfinite" "noalloc"

let to_string i =
  if is_empty i then "[ empty ]"
  else Printf.sprintf "[%.17g, %.17g]" (inf i) (sup i)

let print fmt i =
  if is_empty i then Printf.fprintf fmt "[ empty ]"
  else Printf.fprintf fmt "[%.17g, %.17g]" (inf i) (sup i)

let pretty_print fmt i =
  if is_empty i then Format.fprintf fmt "[ empty ]"
  else if is_point i then Format.fprintf fmt "[ %g ]@," (inf i)
  else Format.fprintf fmt "[%g, %g]@," (inf i) (sup i)

external mid : 'a t -> float = "filib_caml_mid"
external diam : 'a t -> float = "filib_caml_diam"
external rel_diam : 'a t -> float = "filib_caml_relDiam"
external rad : 'a t -> float = "filib_caml_rad"
external mig : 'a t -> float = "filib_caml_mig"
external mag : 'a t -> float = "filib_caml_mag"

external add : 'a t -> 'b t -> rw t = "filib_caml_add"
external sub : 'a t -> 'b t -> rw t = "filib_caml_sub"
external mul : 'a t -> 'b t -> rw t = "filib_caml_mul"
external div : 'a t -> 'b t -> rw t = "filib_caml_div"

external abs : 'a t -> rw t = "filib_caml_abs"
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

external min : 'a t -> 'b t -> rw t = "filib_caml_imin"
external max : 'a t -> 'b t -> rw t = "filib_caml_imax"
external dist : 'a t -> 'b t -> float = "filib_caml_dist"
external blow : 'a t -> float -> rw t = "filib_caml_blow"
external intersect : 'a t -> 'b t -> rw t = "filib_caml_intersect"
external hull : 'a t -> 'b t -> rw t = "filib_caml_hull"
external hull_float : float -> 'a t -> rw t = "filib_caml_hull_float"
(* external hull_float2 : float -> float -> rw t = "filib_caml_hull_float2" *)

external disjoint : 'a t -> 'b t -> bool = "filib_caml_disjoint" "noalloc"
external belongs : float -> 'a t -> bool = "filib_caml_in" "noalloc"
external interior : 'a t -> 'b t -> bool = "filib_caml_interior" "noalloc"
external proper_subset : 'a t -> 'b t -> bool = "filib_caml_proper_subset"
  "noalloc"
external subset : 'a t -> 'b t -> bool = "filib_caml_subset" "noalloc"
external proper_superset : 'a t -> 'b t -> bool = "filib_caml_proper_superset"
  "noalloc"
external superset : 'a t -> 'b t -> bool = "filib_caml_superset" "noalloc"

external seq : 'a t -> 'b t -> bool = "filib_caml_seq"
external sne : 'a t -> 'b t -> bool = "filib_caml_sne"
external sge : 'a t -> 'b t -> bool = "filib_caml_sge"
external sgt : 'a t -> 'b t -> bool = "filib_caml_sgt"
external sle : 'a t -> 'b t -> bool = "filib_caml_sle"
external slt : 'a t -> 'b t -> bool = "filib_caml_slt"

external ceq : 'a t -> 'b t -> bool = "filib_caml_ceq"
external cne : 'a t -> 'b t -> bool = "filib_caml_cne"
external cge : 'a t -> 'b t -> bool = "filib_caml_cge"
external cgt : 'a t -> 'b t -> bool = "filib_caml_cgt"
external cle : 'a t -> 'b t -> bool = "filib_caml_cle"
external clt : 'a t -> 'b t -> bool = "filib_caml_clt"

external peq : 'a t -> 'b t -> bool = "filib_caml_peq"
external pne : 'a t -> 'b t -> bool = "filib_caml_pne"
external pge : 'a t -> 'b t -> bool = "filib_caml_pge"
external pgt : 'a t -> 'b t -> bool = "filib_caml_pgt"
external ple : 'a t -> 'b t -> bool = "filib_caml_ple"
external plt : 'a t -> 'b t -> bool = "filib_caml_plt"


module Do =
struct

  external interval : rw t -> float -> float -> unit
    = "filib_caml_do_interval" "noalloc"
  external inf : rw t -> float -> unit = "filib_caml_do_inf" "noalloc"
  external sup : rw t -> float -> unit = "filib_caml_do_sup" "noalloc"
  external copy : rw t -> 'a t -> unit = "filib_caml_do_copy" "noalloc"

  external pi : rw t -> unit = "filib_caml_do_PI" "noalloc"

  external neg : rw t -> 'a t -> unit = "filib_caml_do_neg" "noalloc"

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

  external abs : rw t -> 'a t -> unit = "filib_caml_do_abs" "noalloc"
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

  external min : rw t -> 'a t -> 'b t -> unit = "filib_caml_do_imin" "noalloc"
  external max : rw t -> 'a t -> 'b t -> unit = "filib_caml_do_imax" "noalloc"
  external blow : rw t -> 'a t -> float -> unit = "filib_caml_do_blow" "noalloc"
  external intersect : rw t -> 'a t -> 'b t -> unit
    = "filib_caml_do_intersect" "noalloc"
  external hull : rw t -> 'a t -> 'b t -> unit = "filib_caml_do_hull" "noalloc"
  external hull_float : rw t -> float -> 'a t -> unit = "filib_caml_do_hull_float"
     "noalloc"
end


external round_downward : unit -> unit = "filib_caml_downward" "noalloc"
external round_upward : unit -> unit = "filib_caml_upward" "noalloc"
external round_to_nearest : unit -> unit = "filib_caml_tonearest" "noalloc"
external round_to_zero : unit -> unit = "filib_caml_tozero" "noalloc"
