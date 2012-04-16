module I = Filib;;
OVERLOAD I = Filib;;

let a = 1
let _ = I.(
  let i1 = interval 0. 1. in
  let i2 = of_float (float a) in
  let i3 = of_float Float.(inf i1 +. inf i2) in
  interval (sup i3) Float.(1.);
)
