(* File: pa_filib.mli

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

val t : Pa_do.Delimited_overloading.t
(** This syntax extension provides a convenient ans {i safe} way of
    writing expressions involving intervals and floating point
    numbers.

    This syntax extension overload the float literals and arithmeric
    operators.  Integer literals and operators keep their default
    interpretation which allows to easily write recursive functions
    whose termination depend on integer arithmetic.

    The syntax extension will introduce temporary variables as
    necessary.  For example [Filib.(1. +. cos 1.)] will first put [cos
    1.] in a temporary variable, then add [1.] to that variable (in an
    optimized way because the literal [1.] is exactly representable as
    a floating point number).  These temporary variables are
    initialized before evaluating the expression ([1. +. cos 1.] in
    the above example).  In case the expression is inside a loop, it
    is thus important to start the overloading outside the loop.
*)
