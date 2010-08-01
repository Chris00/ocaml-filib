(* File: pa_filib.ml

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

open Camlp4.PreCast
open Printf
open Pa_do
open Delimited_overloading

(* debug *)
let print =
  let module P = Camlp4.Printers.OCaml.Make(Syntax)
  in (new P.printer ())#expr Format.err_formatter

let filib = Macro.Module_longident.of_string "Filib"

(* FIXME: some literals may not be represented exactly *)
let t = int empty
  (fun i _ _loc ->
    (* If the integer is not large, do not use 64 bits *)
    let p = if -65536 <= i && i <= 65535 then 16 else 64 in
    <:expr< (Filib.of_float $`int:i$ ~prec:$`int:p$ :> Filib.ro_t) >>)


let () = associate t "Filib"
