open Printf

let () =
  Filib.(
    let onetenth = 0.1 in
    printf "onetenth = %a\n%!" print onetenth;

    let a = copy 0. in
    (* let tmp = Filib.empty() in *)
    for i = 1 to 100_000_000 do
      (* Filib.Do.add a (Filib.cos a); *)
      (* Filib.Do.cos tmp a;  Filib.Do.add a a tmp; *)
      (* Filib.Do.cos tmp a;  Filib.Do.add_to a tmp; *)
      a <- a +. cos(a);
    done;
    printf "sum = %a\n" Filib.print a
  )
