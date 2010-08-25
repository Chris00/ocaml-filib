(* The examples below come from the book

   Introduction to Interval Analysis
   Ramon E. Moore, R. Baker Kearfott, and Michael J. Cloud
   2009 / xii + 223 pages / Softcover / ISBN: 978-0-898716-69-6
   http://www.siam.org/books/ot110/

   They are intended to be executed in the toplevel to get the same
   interactive feel as with MATLAB.  The library is supposed to be
   installed with Findlib (ocamlfind).
*)

#require "filib.syntax";;
OVERLOAD I = Filib;;

(* Example 3.3, p. 22.  The syntax extension makes sure that float
   literals are exactly representable, otherwise an error will be
   raised with a suggestion to use [hull] instead.  So, in contrast to
   the caution of page 25 — whose pertinence is illustrated by several
   examples in http://www.springerlink.com/content/q238114655250357/ —
   there is no risk in using literals with this library. *)

let wide_V, narrow_V =
  I.(let g = interval 1.32710e20 1.32715e20 in
     let v0 = interval 2.929e4 3.029e4 in
     let m = interval 2.066e11 2.493e11 in
     let e = interval 1.470e11 1.521e11 in
     let wide_result = 2. *. g *. m /. (e *. (m +. e)) in
     sqrt wide_result -. v0,  sqrt(2. *. g /. (e *. (1. +. e/.m))) -. v0 )
;;

(* To see more digits, convert it to a string *)
I.(to_string narrow_V);;

