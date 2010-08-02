open Printf

let () =
  (* FIXME: this must use the syntax extension: *)
  let onetenth = Filib.of_float 0.1 in

  let a = Filib.of_float 0. in
  for i = 1 to 10_000_000 do
    (* Filib.Do.add a onetenth; *)
    Filib.Do.add_float a 1.;
  done;
  printf "ocaml: %s\n" (Filib.to_string a)
