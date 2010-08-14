open Printf

let () =
  Filib.(
    let onetenth = 0.1 in
    printf "onetenth = %a\n%!" print onetenth;

    let a = copy 0. in
    for i = 1 to 100_000_000 do
      (* Filib.Do.add_float a 0.1; *)
      a <- a +. onetenth;
    done;
    printf "sum = %a\n" Filib.print a
  )
