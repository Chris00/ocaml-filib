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

external init : unit -> unit = "filib_caml_init" "noalloc"

let () = init()

external empty : unit -> rw t = "filib_caml_EMPTY"
external entire : unit -> rw t = "filib_caml_ENTIRE"
external neg_infinity : unit -> rw t = "filib_caml_NEG_INFTY"
external infinity : unit -> rw t = "filib_caml_POS_INFTY"
external zero : unit -> rw t = "filib_caml_ZERO"
external one : unit -> rw t = "filib_caml_ONE"
external pi : unit -> rw t = "filib_caml_PI"

external of_float : float -> rw t = "filib_caml_of_float"
external interval : float -> float -> rw t = "filib_caml_interval"
external copy : 'a t -> rw t = "filib_caml_copy"

external inf : 'a t -> float = "filib_caml_inf"
external sup : 'a t -> float = "filib_caml_sup"

let is_nan (x: float) = (x <> x)

let is_empty i = is_nan(inf(i)) && is_nan(sup(i))

let to_string i =
  Printf.sprintf "[%.16e, %.16e]" (inf i) (sup i)

let print fmt i =
  Format.fprintf fmt "[%.16e, %.16e]" (inf i) (sup i)



module Do =
struct

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
