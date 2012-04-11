open Printf
module I = Filib;;
OVERLOAD I = Filib;;

let () = I.(
  let l = ref [] in
  let a = ref 1. in
  for i = 1 to 5 do
    let x = float i
    and y = float (i + 1) in
    let i = interval x y in
    l := i :: !l;
  done;
  List.iter (fun i -> printf "%s\n%!" (to_string i)) !l
)
