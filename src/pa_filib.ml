(* File: pa_filib.ml

   Copyright (C) 2010

     Christophe Troestler <Christophe.Troestler@umons.ac.be>
     WWW: http://math.umons.ac.be/an/software/

   This library is free software; you can redistribute it and/or modify
   it under the terms of the GNU Lesser General Public License version 3 or
   later as published by the Free Software Foundation, with the special
   exception on linking described in the file LICENSE.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
   LICENSE for more details. *)

open Camlp4.PreCast
open Printf
open Pa_do
open Delimited_overloading

(* debug *)
let print =
  let module P = Camlp4.Printers.OCaml.Make(Syntax)
  in (new P.printer ())#expr Format.err_formatter

let filib = Macro.Module_longident.of_string "Filib"
let filib_do = Macro.Module_longident.of_string "Filib.Do"

let bin_rels = [ "="; "<>"; "<"; "<="; ">"; ">="]
let bin_ops_infix = [ "+."; "-."; "*."; "/."; "**"]
let bin_ops_prefix = [ "min"; "max"; "hull"; "interval" ]
let bin_ops = bin_ops_infix @ bin_ops_prefix

let unary_ops_float = [ (* unary operators that return a float *)
  "inf"; "sup"; "mid"; "diam"; "rel_diam"; "rad"; "mig"; "mag" ]
let unary_ops_ascii = [
  "abs"; "acos"; "acosh"; "acoth"; "asin"; "asinh"; "atan"; "atanh";
  "cos"; "cosh"; "cot"; "coth"; "exp"; "exp10"; "exp2"; "expm1";
  "log"; "log10"; "log1p"; "log2"; "sin"; "sinh"; "sqr"; "sqrt";
  "tan"; "tanh" ] @ unary_ops_float

let unary_ops = "~-." :: unary_ops_ascii

let open_for = [
  "empty"; "entire"; "zero"; "one"; "pi"; "infinity"; "neg_infinity";
  "of_float"; "interval"; "copy";
  "to_string"; "print"; "pretty_print";
  "is_empty"; "is_point"; "is_infinite" ]
  @ bin_ops_prefix @ unary_ops_ascii


(** Functions to manage sets of temporary variables needed to compute
    expressions. *)
module Var =
struct
  let available_vars = ref []
  (* Global var holding the temporary variables for the current
     expression (in which overloadings are resolved).  If [Filib.(e)]
     occurs inside [Filib.()] overloading, all temporary variables
     will be put before the external overloading. *)
  let declared_vars = ref []
  (* All temp vars, even those which cannot be reused (are only used
     for a single "toplevel" expression).  Superset of [available_vars]. *)

  let level = ref 0
  (* Cound the number of nested overloadings for Filib. *)

  let init() =
    assert(!available_vars = [] && !declared_vars = []);
    incr level

  let clear() =
    declared_vars := [];
    available_vars := []

  let letin e =
    decr level; (* leave the current overloading *)
    if !level = 0 then (* outer overloading => declare vars *)
      let declare e v =
        let _loc = Ast.loc_of_expr e in
        <:expr< let $lid:v$ = Filib.empty() in $e$ >> in
      let e = List.fold_left declare e !declared_vars in
      clear();
      e
    else e

  (** Add the setup to the set of overloadings [t] to be able to use
      this module. *)
  let setup t =
    let t = before t init in
    after t letin

  (** [use f] perform [f v] where [v] is picked in available temporary
      variables (or created anew).  If [once], then the variable is
      not added to the list of temporary variables (it may be, for
      example, because it is further used in the program). *)
  let use ?(once=false) f =
    let new_var = match !available_vars with
      | [] ->
        let v = new_lid() (* Create a new variable *) in
        declared_vars := v :: !declared_vars;
        v
      | v :: tl -> available_vars := tl; v in
    let e = f new_var in
    if not once then available_vars := new_var :: !available_vars;
    e

  (** Add the variable [v] to the list of temporaries for performing
      [f] and remove it after (as [v] may not be in the scope for
      other expressions).  It is expected that [v] needs not to be
      initialized. *)
  let add v f =
    available_vars := v :: !available_vars;
    let e = f() in
    available_vars := List.tl !available_vars;
    e
end

(** The proper way to raise exceptions in this syntax extension (to
    play well in the toploop). *)
let raise loc e =
  (* When an exception is raised, the temp vars will not be declared
     but the next toplevel expression must start in a clean state. *)
  Var.clear();
  Var.level := 0;
  Loc.raise loc e


type expr =
| Float of Loc.t * string      (* float constant *)
| Var of Loc.t * string        (* variable (lowercase identifier) *)
| Op1 of Loc.t * string * expr (* unary operation (inclusing "~-.") *)
| Op2 of Loc.t * string * expr * expr (* binary op, including "+",... *)
| Unknown of Ast.expr          (* unknown term *)

let loc_of_expr = function
  | Float(l,_) | Var(l,_) | Op1(l,_,_) | Op2(l,_,_,_) -> l
  | Unknown e -> Ast.loc_of_expr e

let binary_do_op _loc lid = match lid with
  (* specializations used below *)
  | "+." -> <:expr< Filib.Do.add >>
  | "-." -> <:expr< Filib.Do.sub >>
  | "*." -> <:expr< Filib.Do.mul >>
  | "/." -> <:expr< Filib.Do.div >>
  | _ -> qualify_lid lid filib_do _loc


(* Transformation of float constants to intervals
 ***********************************************************************)
(* The transformation of decimal FP numbers to binary IEEE-754 format
   performed here is fairly naive.  It could be made more efficient by
   using the following papers:

   William D. Clinger. How to read floating point numbers accurately. In
   PLDI, pages 92–101. ACM, 1990. ISBN 0-89791-364-7.

   David M. Gay, Correctly Rounded Binary-Decimal and Decimal-Binary
   Conversions. Numerical Analysis Manuscript 90-10, AT&T Bell
   Laboratories, Murray Hill, NJ (1990).  *)

let emin = -1022 (* correspond to 1 *)
let emax = 1023  (* correspond to 2046 *)

(** Parse a literal float and returns the corresponding num. *)
let num_of_literal lit =
  let len = String.length lit in
  let neg = lit.[0] = '-' in
  let start = if neg then 1 else 0 in
  let e =
    try String.index lit 'e'
    with Not_found ->
      try String.index lit 'E' with Not_found -> len in
  (* Beware that, with the scientific notation, there is no garantee
     of a dot in the literal (e.g. "1e3" is OK). *)
  let dot, decimals =
    try  let d = String.index lit '.' in d, e - 1 - d
    with Not_found -> e, 0 in
  let m1 = String.sub lit start (dot - start) in
  let m =
    if decimals > 0 then m1 ^ String.sub lit (dot + 1) decimals else m1 in
  let mantissa = Num.(of_string m / (10**(of_int decimals))) in
  let n =
    if e = len then mantissa
    else
      let e = int_of_string (String.sub lit (e + 1) (len - e - 1)) in
      Num.(mantissa * 10**(of_int e))in
  (neg, n)

let rec exponent_pos n e =
  if Num.(n < 2) then e, n
  else exponent_pos Num.(n / 2) (e + 1)

let rec exponent_neg n e =
  if Num.(n >= 1) then e, n
  else exponent_neg Num.(n * 2) (e - 1)

(** return the number [e] such that [m = n * 2**(-e) ∈ [1,2[].  Returns
    also the manstissa [m]. *)
let exponent n =
  assert(Num.(n > 0));
  if Num.(n >= 1) then exponent_pos n 0
  else exponent_neg n 0

let shift_mantissa = Num.(2**52)

let inf_sup lit =
  let neg, n = num_of_literal lit in
  (* Sign bit *)
  let f = if neg then 0x8000000000000000L else 0x0L in
  if Num.(n = 0) then f, f
  else begin
    let e, m = exponent n in
    (* FIXME: denormalized numbers and infinites *)
    (* eprintf ">>> %s => e = %i, m = %s " lit e Num.(to_string m); *)
    let e = e - emin + 1 in
    let f = Int64.(f lor (of_int e lsl 52)) in
    let m = Num.((m - 1) * shift_mantissa) in
    let m_inf = Num.(to_int(floor m))
    and m_sup = Num.(to_int(ceil m)) in
    (* eprintf "in [%i, %i] * 2^-52\n" m_inf m_sup; *)
    let f_inf = Int64.(f lor (of_int m_inf))
    and f_sup = Int64.(f lor (of_int m_sup)) in
    if neg then f_sup, f_inf else f_inf, f_sup
  end

let saved_point_intervals = Hashtbl.create 10

let is_exactly_representable lit =
  let inf, sup = inf_sup lit in Int64.(inf = sup)

let exact_representation _loc x_lit =
  let inf, sup = inf_sup x_lit in
  if Int64.(inf = sup) then (* representation of [x_lit] is exact *)
    if !Sys.interactive then
      <:expr< Int64.float_of_bits $`int64:inf$ >>
    else (
      try
        <:expr< $lid:Hashtbl.find saved_point_intervals x_lit$ >>
      with Not_found ->
        let x = new_lid() in
        add_to_beginning_of_file
          (<:str_item< let $lid:x$ = Int64.float_of_bits $`int64:inf$ >>);
        Hashtbl.add saved_point_intervals x_lit x;
        <:expr< $lid:x$ >>
    )
  else
    let msg = sprintf "The number %s is not exactly representable as a \
	floating point.  Use \"hull\" instead of \"interval\"" x_lit in
    raise _loc (Stream.Error msg)

let saved_const_intervals = Hashtbl.create 10

(** [const_interval x_lit] return the variable name of the constant
    interval in which [x_lit] is stored.  It caches the transformation
    at the beginning of the file. *)
let const_interval _loc x_lit =
  let x = new_lid() in
  if !Sys.interactive then
    (* In the toploop one must declare the constant on the spot. *)
    let x_inf, x_sup = inf_sup x_lit in
    let a = new_lid() and b = new_lid() in
    <:expr< let $lid:a$ = Int64.float_of_bits $`int64:x_inf$ in
            let $lid:b$ = Int64.float_of_bits $`int64:x_sup$ in
            (Filib.interval $lid:a$ $lid:b$ :> Filib.ro_t) >>
  else (
    try
      let x = Hashtbl.find saved_const_intervals x_lit in
      <:expr< $lid:x$ >> (* use the current location *)
    with Not_found ->
      let x_inf, x_sup = inf_sup x_lit in
      add_to_beginning_of_file
        (<:str_item<
            let $lid:x$ =
              let x_inf = Int64.float_of_bits $`int64:x_inf$ in
              let x_sup = Int64.float_of_bits $`int64:x_sup$ in
              (Filib.interval x_inf x_sup :> Filib.ro_t)
              >> );
      Hashtbl.add saved_const_intervals x_lit x;
      <:expr< $lid:x$ >>
  )
;;

(* Overloading
 ***********************************************************************)

(* We cannot use the [Delimited_overloading.float] function because it
   returns the float already converted while we need it in literal
   form to be able to map it to an interval. *)

let t = Var.setup empty

(** Evaluate [e] a single time, possibly putting it in a variable [v]
    of [Var] and execute [f v] -- typically to generate code for an
    expression depending on [e]. *)
let rec use_var_for ?once e f =
  match e with
  | Float(_loc, x) -> f (const_interval _loc x)
  | Var(_loc, v) -> f <:expr< $lid:v$ >> (* use the var *)
  | Op1(_loc, op, e) when List.mem op unary_ops_float ->
    (* If [op] returns a float, we do not need to hold its value in a var *)
    f (use_var_for e (fun e -> <:expr< $qualify_lid op filib _loc$ $e$ >>))
  | _ ->
    let _loc = loc_of_expr e in
    Var.use ?once (fun v ->
      let code_f = f <:expr< $lid:v$ >> in
      (* [v] is a fresh temporary var, it contains no important value
         and is not used in [e], so one can use it in the computations
         before setting its value. *)
      <:expr< $set v e ~reuse_var:true$;  $code_f$ >>)

(** @return the code to do [res <- e]. *)
and set ?(reuse_var=false) res e =
  if reuse_var then Var.add res (fun () -> set_with_result res e)
  else set_with_result res e
and set_with_result res e =
  match e with
  | Op1(_loc, lid, e) ->
    let op = match lid with
      | "~-." -> <:expr< Filib.Do.neg >>
      | _ -> qualify_lid lid filib_do _loc in
    use_var_for e (fun e -> <:expr< $op$ $lid:res$ $e$ >>)

  (* Specialized functions for +,-,*,/ on literals *)
  | Op2(_loc, "+.", e, Float(locx, x)) | Op2(_loc, "+.", Float(locx, x), e)
      when is_exactly_representable x ->
    let x = exact_representation locx x in
    use_var_for e (fun e -> <:expr< Filib.Do.add_float $lid:res$ $e$ $x$ >>)
  | Op2(_loc, "*.", e, Float(locx, x)) | Op2(_loc, "*.", Float(locx, x), e)
      when is_exactly_representable x ->
    let x = exact_representation locx x in
    use_var_for e (fun e -> <:expr< Filib.Do.mul_float $lid:res$ $e$ $x$ >>)
  | Op2(_loc, "-.", e, Float(locx, x)) when is_exactly_representable x ->
    let x = exact_representation locx x in
    use_var_for e (fun e -> <:expr< Filib.Do.sub_float $lid:res$ $e$ $x$ >>)
  | Op2(_loc, "-.", Float(locx, x), e) when is_exactly_representable x ->
    let x = exact_representation locx x in
    use_var_for e (fun e -> <:expr< Filib.Do.float_sub $lid:res$ $x$ $e$ >>)
  | Op2(_loc, "/.", e, Float(locx, x)) when is_exactly_representable x ->
    let x = exact_representation locx x in
    use_var_for e (fun e -> <:expr< Filib.Do.div_float $lid:res$ $e$ $x$ >>)
  | Op2(_loc, "/.", Float(locx, x), e) when is_exactly_representable x ->
    let x = exact_representation locx x in
    use_var_for e (fun e -> <:expr< Filib.Do.float_div $lid:res$ $x$ $e$ >>)

  (* Exponentiation *)
  | Op2(_loc, "**", e, Float(locx, x_lit)) ->
    let x = float_of_string x_lit in
    if x = 2. then
      use_var_for e (fun e -> <:expr< Filib.Do.sqr $lid:res$ $e$ >>)
    else
      let trunc_x = truncate x in
      if x = float_of_int trunc_x then
        use_var_for e (fun e ->
          <:expr< Filib.Do.power $lid:res$ $e$ $`int:trunc_x$ >>)
      else
        let x = const_interval locx x_lit in
        use_var_for e (fun e -> <:expr< Filib.Do.pow $lid:res$ $e$ $x$ >>)

  (* Interval creation.  Do not introduce variables for the float
     arguments but overload them as [interval (inf x) (mid y)] is possible. *)
  | Op2(_loc, "interval", Float(locx, x), Float(locy, y)) ->
    let x = exact_representation locx x in
    let y = exact_representation locy y in
    <:expr< Filib.Do.interval $lid:res$ $x$ $y$ >>

  (* Binary operations (general case) *)
  | Op2(_loc, op, Var(locv, v), e) ->
    let v = <:expr@locv< $lid:v$ >> in
    use_var_for e (fun e -> <:expr< $binary_do_op _loc op$ $lid:res$ $v$ $e$ >>)
  | Op2(_loc, op, e, Var(locv, v)) ->
    let v = <:expr@locv< $lid:v$ >> in
    use_var_for e (fun e -> <:expr< $binary_do_op _loc op$ $lid:res$ $e$ $v$ >>)
  | Op2(_loc, op, e1, e2) ->
    use_var_for e1 (fun e1 ->
      use_var_for e2 (fun e2 ->
        <:expr< $binary_do_op _loc op$ $lid:res$ $e1$ $e2$ >>))

  | Float(_loc, x) -> const_interval _loc x
  | Var(_loc, v) ->
    if res = v then <:expr< >> (* nothing to do *)
    else <:expr< Filib.Do.copy $lid:res$ $lid:v$ >>
  | Unknown e ->
    let _loc = Ast.loc_of_expr e in
    <:expr< Filib.Do.copy $lid:res$ $e$ >>


(* Parse an interval expression. *)
let rec parse_expr tr = function
  | <:expr@loc< $flo:x$ >> -> Float(loc, x)
  | <:expr@loc< $lid:op$ $e1$ $e2$ >>
      when List.mem op bin_ops || List.mem op bin_rels ->
    Op2(loc, op, parse_expr tr e1, parse_expr tr e2)
  | <:expr@loc< $lid:op$ $e1$ >> when List.mem op unary_ops ->
    Op1(loc, op, parse_expr tr e1)
  | <:expr@loc< $lid:op$ >> -> Var(loc, op)
        (* FIXME: the notion of variable should be generalized e.g. to x.z *)
  | e -> Unknown((self tr)#expr e)

let specialize tr expr =
  let _loc = Ast.loc_of_expr expr in
  match expr with
  | <:expr@loc< $flo:x$ >> -> const_interval loc x
  (* Assignment *)
  | <:expr< $lid:r$ <- $e$ >> ->
    (* The value of [r] may be used in the expression [e], we cannot
       use it as a temporary variable. *)
    set r (parse_expr tr e) ~reuse_var:false
  (* Unary ops *)
  | <:expr< $lid:f$ $e$ >> when List.mem f unary_ops ->
    (* The temporary var will hold the final value of the expression *)
    use_var_for ~once:true (Op1(_loc, f, parse_expr tr e)) (fun e -> e)
  (* binary operators (+, ...) *)
  | <:expr< $lid:f$ $e1$ $e2$ >> when List.mem f bin_ops ->
    use_var_for ~once:true (Op2(_loc, f, parse_expr tr e1, parse_expr tr e2))
      (fun e -> e)
  (* binary relations (<, ...) *)
  | <:expr< $lid:f$ $e1$ $e2$ >> when List.mem f bin_rels ->
    (* FIXME: do we allow a configurable policy ? *)
    use_var_for (parse_expr tr e1) (fun e1 ->
      use_var_for (parse_expr tr e2) (fun e2 ->
        match f with
        | "=" ->  <:expr< Filib.seq $e1$ $e2$ >>
        | "<>" -> <:expr< Filib.sne $e1$ $e2$ >>
        | "<" ->  <:expr< Filib.clt $e1$ $e2$ >>
        | "<=" -> <:expr< Filib.cle $e1$ $e2$ >>
        | ">" ->  <:expr< Filib.cgt $e1$ $e2$ >>
        | ">=" -> <:expr< Filib.cge $e1$ $e2$ >>
        | _ -> assert false (* see [bin_rels] *)
      ))

  (* Explicitely open the module for the functions of the functional
     interface (the ones of the imperative interface are supposed to
     be used through the "<-" notation) *)
  | <:expr@_loc< $lid:f$ >> when List.mem f open_for ->
    qualify_lid f filib _loc

  | _ -> super tr expr


let t = expr t specialize


let () = associate t "Filib"
