open Printf

let () =
  Filib.(
    let onetenth = 1. /. 10. in
    printf "onetenth = %a\n" print onetenth;

    let a = 0. in
    let tmp = Filib.empty() in
    for i = 1 to 10_000_000 do
      Filib.Do.cos tmp a;  Filib.Do.add a a tmp;
    (* Filib.Do.cos tmp a;  Filib.Do.add_to a tmp; *)
    (* Filib.Do.add a (Filib.cos a); *)
    (* Filib.Do.add_float a 0.1; *)
    done;
    printf "sum = %a\n" Filib.print a
  )
