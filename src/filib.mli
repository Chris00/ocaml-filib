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
