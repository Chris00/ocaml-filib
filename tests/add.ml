open Printf

let () =
  (* FIXME: this must use the syntax extension: *)
  let onetenth = Filib.of_float 0.1 in

  let a = Filib.of_float 0. in
  let tmp = Filib.empty() in
  for i = 1 to 10_000_000 do
    Filib.Do.cos tmp a;
    Filib.Do.add a tmp;
    (* Filib.Do.add a (Filib.cos a); *)
    (* Filib.Do.add_float a 0.1; *)
  done;
  printf "sum = %a\n" Filib.print a
