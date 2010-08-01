(* File: filib_top.ml

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

let print_outcome = false
let error_fmt = Format.err_formatter

let eval_phrase s =
  let lexbuf = Lexing.from_string s in
  let phrase = !Toploop.parse_toplevel_phrase lexbuf in
  Toploop.execute_phrase print_outcome error_fmt phrase

let () =
  let success = eval_phrase "#install_printer Filib.print;;" in
  if not success then (
    Format.fprintf error_fmt
      "Something weird appened while installing Filib library printer";
    Format.pp_print_flush error_fmt ()
  )
