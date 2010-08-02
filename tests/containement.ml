open Printf
module I = Filib

let () =
  let x = I.interval (-1.5) 3. in
  printf "s=%a\n" I.print x;
  printf "cos(x) = %a\n" I.print (I.cos x);
  printf "log(x) = %a\n" I.print (I.log x);
  printf "atan(log(x)) = %a\n" I.print (I.atan(I.log(x)));
  printf "log([-2,-1]) = %a\n" I.print (I.log(I.interval (-2.) (-1.)));

  let x = I.entire() in
  printf "x = %a,\trad(x) = %g\n" I.print x (I.rad x);
  printf "is_infinite x = %b\n" (I.is_infinite x);
  printf "cosh(sqrt(log(x - 0.5)) / sin(x)) = %a\n" I.print
    (I.cosh(I.div (I.sqrt(I.log(I.sub x (I.of_float 0.5)))) (I.sin(x))))
