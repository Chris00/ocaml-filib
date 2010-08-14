(* Test about reusing vars *)

open Printf

let sin1 = Filib.(sin 1.)

let () =
  Filib.(
    let x = sin 1. +. cos 1. in (* requires 2 temp vars, one can be
                                   used for the final result. *)
    let y = cos 1. in (* could reuse the temp var which was not used
                         for holding [x]. *)
    (* CHECK: If the temporary variable containing the value of x is
       (wrongly) reused for y, the value of x will be modified. *)
    if x = sin1 then printf "test passed.\n"
    else printf "test failed.\n"
  )

