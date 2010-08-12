(* Test about reusing vars *)

open Printf

let sin1 = Filib.(sin 1.)

let () =
  Filib.(
    let x = sin 1. in
    let y = cos 1. in
    (* If the temporary variable containing the value of x is reused
       for y, the value of x will be modified. *)
    if x = sin1 then printf "test passed.\n"
    else printf "test failed.\n"
  )

