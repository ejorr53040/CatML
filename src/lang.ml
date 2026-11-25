(*
   CatML Abstract Syntax
   ----------------------
*)
type var = string

type expr =
  (* boolean expression forms *)
  | Bool of bool
  | And of expr * expr
  | Or of expr * expr
  | Not of expr
  (* arithmetic expression forms *)
  | Int of int
  | Plus of expr * expr
  | Mult of expr * expr
  | Equal of expr * expr
  (* conditional expression form *)
  | If of expr * expr * expr
  (* Variables and let expressions *)
  | Var of var
  | Let of var * expr * expr
  (*pairs functions and fix*)
  | Pair of expr * expr
  | First of expr
  | Second of expr
  | Fun of var * expr
  | Fix of var * var * expr
  | App of expr * expr

(*
  is_val : expr -> bool
  in : AST [[e]]
  out : true iff e is a value
*)
let rec is_val e =
  match e with
  | Int _ -> true
  | Bool _ -> true
  | Pair (e1, e2) -> is_val e1 && is_val e2
  | Fun (_, _) -> true
  | Fix (_, _, _) -> true
  | _ -> false

(*************************)
(* Operational Semantics *)
(*************************)

(* Define substitution *)
let rec subst (x, v, e) : expr =
  match e with
  | Var y -> if x = y then v else Var y
  | Let (y, e1, e2) ->
      Let (y, subst (x, v, e1), if x = y then e2 else subst (x, v, e2))
  | Bool _ | Int _ -> e
  | And (e1, e2) -> And (subst (x, v, e1), subst (x, v, e2))
  | Or (e1, e2) -> Or (subst (x, v, e1), subst (x, v, e2))
  | Not e' -> Not (subst (x, v, e'))
  | Plus (e1, e2) -> Plus (subst (x, v, e1), subst (x, v, e2))
  | Mult (e1, e2) -> Mult (subst (x, v, e1), subst (x, v, e2))
  | Equal (e1, e2) -> Equal (subst (x, v, e1), subst (x, v, e2))
  | If (e0, e1, e2) -> If (subst (x, v, e0), subst (x, v, e1), subst (x, v, e2))
  | Pair (e1, e2) -> Pair (subst (x, v, e1), subst (x, v, e2))
  | First e' -> First (subst (x, v, e'))
  | Second e' -> Second (subst (x, v, e'))
  | Fun (y, e') -> Fun (y, if x = y then e' else subst (x, v, e'))
  | Fix (f, y, e') -> Fix (f, y, if x = y || x = f then e' else subst (x, v, e'))
  | App (e1, e2) -> App (subst (x, v, e1), subst (x, v, e2))

type step_result = Step of expr | NotReducible

(* 
   step : expr -> step_result
   in : An expression [[e]]
   out : An expression [[e']] if e -> e' in the operational semantics;
         if no such [[e']] exists, returns [[NotReducible]]
*)
let rec step (e : expr) : step_result =
  match e with
  | Bool _ | Int _ -> NotReducible
  | Not (Bool b) -> Step (Bool (not b))
  | Not e1 ->
      begin match step e1 with
      | Step e1' -> Step (Not e1')
      | NotReducible -> NotReducible
      end
  | And (Bool b1, Bool b2) -> Step (Bool (b1 && b2))
  | And (e1, e2) ->
      begin match step e1 with
      | Step e1' -> Step (And (e1', e2))
      | NotReducible ->
          begin match step e2 with
          | Step e2' -> Step (And (e1, e2'))
          | NotReducible -> NotReducible
          end
      end
  | Or (Bool b1, Bool b2) -> Step (Bool (b1 || b2))
  | Or (e1, e2) ->
      begin match step e1 with
      | Step e1' -> Step (Or (e1', e2))
      | NotReducible ->
          begin match step e2 with
          | Step e2' -> Step (Or (e1, e2'))
          | NotReducible -> NotReducible
          end
      end
  | Plus (Int n1, Int n2) -> Step (Int (n1 + n2))
  | Plus (e1, e2) ->
      begin match step e1 with
      | Step e1' -> Step (Plus (e1', e2))
      | NotReducible ->
          begin match step e2 with
          | Step e2' -> Step (Plus (e1, e2'))
          | NotReducible -> NotReducible
          end
      end
  | Mult (Int n1, Int n2) -> Step (Int (n1 * n2))
  | Mult (e1, e2) ->
      begin match step e1 with
      | Step e1' -> Step (Mult (e1', e2))
      | NotReducible ->
          begin match step e2 with
          | Step e2' -> Step (Mult (e1, e2'))
          | NotReducible -> NotReducible
          end
      end
  | Equal (Int n1, Int n2) -> Step (Bool (n1 = n2))
  | Equal (e1, e2) ->
      begin match step e1 with
      | Step e1' -> Step (Equal (e1', e2))
      | NotReducible ->
          begin match step e2 with
          | Step e2' -> Step (Equal (e1, e2'))
          | NotReducible -> NotReducible
          end
      end
  | If (Bool true, e1, _) -> Step e1
  | If (Bool false, _, e2) -> Step e2
  | If (e1, e2, e3) ->
      begin match step e1 with
      | Step e1' -> Step (If (e1', e2, e3))
      | NotReducible -> NotReducible
      end
  | Var _ -> NotReducible
  | Let (x, e1, e2) ->
      begin match step e1 with
      | Step e1' -> Step (Let (x, e1', e2))
      | NotReducible ->
          if is_val e1 then Step (subst (x, e1, e2)) else NotReducible
      end
  | Pair (e1, e2) ->
      begin match step e1 with
      | Step e1' -> Step (Pair (e1', e2))
      | NotReducible ->
          begin match step e2 with
          | Step e2' -> Step (Pair (e1, e2'))
          | NotReducible -> NotReducible
          end
      end
  | First e ->
      begin match step e with
      | Step e' -> Step (First e')
      | NotReducible ->
          begin match e with
          | Pair (v1, v2) when is_val e -> Step v1
          | _ -> NotReducible
          end
      end
  | Second e ->
      begin match step e with
      | Step e' -> Step (Second e')
      | NotReducible ->
          begin match e with
          | Pair (v1, v2) when is_val e -> Step v2
          | _ -> NotReducible
          end
      end
  | Fun _ | Fix _ -> NotReducible
  | App (e1, e2) ->
      begin match step e1 with
      | Step e1' -> Step (App (e1', e2))
      | NotReducible ->
          begin match step e2 with
          | Step e2' -> Step (App (e1, e2'))
          | NotReducible ->
              begin match e1 with
              | Fun (x, body) when is_val e2 -> Step (subst (x, e2, body))
              | Fix (f, x, body) when is_val e2 ->
                  Step (subst (f, e1, subst (x, e2, body)))
              | _ -> NotReducible
              end
          end
      end

(*
   multistep : expr -> expr
   in : AST [[e]]
   out : [[e']] such that e ->* e' -/-> in the operational semantics
*)
let rec multistep e =
  match step e with Step e' -> multistep e' | NotReducible -> e

(*****************)
(* Type checking *)
(*****************)

type arith_type =
  | INT
  | BOOL
  | ARROW of arith_type * arith_type
  | PAIR of arith_type * arith_type
  | TVAR of int

(* Define a type to represent finite maps/typing contexts *)
type context = (var * arith_type) list

(* empty_context : context *)
let empty_context : context = []

(* lookup : context * var -> arith_type option
 * input :
 *    g - a typing context
 *    x - a variable
 * output :
 *    if (x,tau) is in g, return Some tau
 *    otherwise, return None
 *)
let rec lookup (g, x) : arith_type option =
  match g with
  | [] -> None
  | (y, tp) :: g' -> if x = y then Some tp else lookup (g', x)

(* extend : context * var * arith_type -> context
 * input:
 *    g - a typing context
 *    x - a variable
 *    tp - an arith_type
 * output:
 *    A typing context that maps x to tp and any other y to g(y)
 *)
let extend (g, x, tp) : context = (x, tp) :: g

(*Some type inferrence ec stuff *)
let fresh n : arith_type * int = (TVAR n, n + 1)

let rec apply_subst s t =
  match t with
  | INT | BOOL -> t
  | TVAR n ->
      begin match List.assoc_opt n s with
      | None -> TVAR n
      | Some t -> apply_subst s t
      end
  | PAIR (t1, t2) -> PAIR (apply_subst s t1, apply_subst s t2)
  | ARROW (t1, t2) -> ARROW (apply_subst s t1, apply_subst s t2)

(*unify helper*)
let rec occurs n t =
  match t with
  | TVAR m -> m = n
  | INT -> false
  | BOOL -> false
  | ARROW (l, r) -> occurs n r || occurs n l
  | PAIR (l, r) -> occurs n l || occurs n r

let rec unify (s, t1, t2) : (int * arith_type) list option =
  match (apply_subst s t1, apply_subst s t2) with
  | INT, INT | BOOL, BOOL -> Some s
  | TVAR a, TVAR b when a = b -> Some s
  | TVAR n, t | t, TVAR n -> if occurs n t then None else Some ((n, t) :: s)
  | ARROW (a1, b1), ARROW (a2, b2) -> (
      match unify (s, a1, a2) with None -> None | Some s' -> unify (s', b1, b2))
  | PAIR (a1, b1), PAIR (a2, b2) -> (
      match unify (s, a1, a2) with None -> None | Some s' -> unify (s', b1, b2))
  | _ -> None

let rec infer (g, s, n, e) : (arith_type * (int * arith_type) list * int) option
    =
  match e with
  | Var x ->
      begin match lookup (g, x) with Some t -> Some (t, s, n) | None -> None
      end
  | Let (x, e1, e2) ->
      begin match infer (g, s, n, e1) with
      | None -> None
      | Some (t1, s1, n1) -> infer (extend (g, x, t1), s1, n1, e2)
      end
  | Bool _ -> Some (BOOL, s, n)
  | And (e1, e2) | Or (e1, e2) ->
      begin match infer (g, s, n, e1) with
      | None -> None
      | Some (t1, s1, n1) ->
          begin match unify (s1, t1, BOOL) with
          | None -> None
          | Some s2 ->
              begin match infer (g, s2, n1, e2) with
              | None -> None
              | Some (t2, s3, n2) ->
                  begin match unify (s3, t2, BOOL) with
                  | None -> None
                  | Some s4 -> Some (BOOL, s4, n2)
                  end
              end
          end
      end
  | Not e' ->
      begin match infer (g, s, n, e') with
      | None -> None
      | Some (t1, s1, n1) ->
          begin match unify (s1, t1, BOOL) with
          | None -> None
          | Some s2 -> Some (BOOL, s2, n1)
          end
      end
  | Int _ -> Some (INT, s, n)
  | Plus (e1, e2) | Mult (e1, e2) ->
      begin match infer (g, s, n, e1) with
      | None -> None
      | Some (t1, s1, n1) ->
          begin match unify (s1, t1, INT) with
          | None -> None
          | Some s2 ->
              begin match infer (g, s2, n1, e2) with
              | None -> None
              | Some (t2, s3, n2) ->
                  begin match unify (s3, t2, INT) with
                  | None -> None
                  | Some s4 -> Some (INT, s4, n2)
                  end
              end
          end
      end
  | Equal (e1, e2) ->
      begin match infer (g, s, n, e1) with
      | None -> None
      | Some (t1, s1, n1) ->
          begin match unify (s1, t1, INT) with
          | None -> None
          | Some s2 ->
              begin match infer (g, s2, n1, e2) with
              | None -> None
              | Some (t2, s3, n2) ->
                  begin match unify (s3, t2, INT) with
                  | None -> None
                  | Some s4 -> Some (BOOL, s4, n2)
                  end
              end
          end
      end
  | If (e0, e1, e2) ->
      begin match infer (g, s, n, e0) with
      | None -> None
      | Some (t1, s1, n1) ->
          begin match unify (s1, t1, BOOL) with
          | None -> None
          | Some s2 ->
              begin match infer (g, s2, n1, e1) with
              | None -> None
              | Some (t2, s3, n2) ->
                  begin match infer (g, s3, n2, e2) with
                  | None -> None
                  | Some (t3, s4, n3) ->
                      begin match unify (s4, t2, t3) with
                      | None -> None
                      | Some s5 -> Some (t2, s5, n3)
                      end
                  end
              end
          end
      end

(********************************************)
(* Utility functions for working with ARITH *)
(********************************************)

(*
  string_of_expr : expr -> string
  in : An expression AST [[e]].
  out : The concrete expression e in string format.
*)
let rec string_of_expr e =
  match e with
  (* TODO: extend string_of_expr *)
  | Var x -> x
  | Let (x, e1, e2) ->
      "Let " ^ x ^ " = " ^ string_of_expr e1 ^ " In " ^ string_of_expr e2
  | Bool true -> "True"
  | Bool false -> "False"
  | Int n -> string_of_int n
  | And (e1, e2) -> "(" ^ string_of_expr e1 ^ " And " ^ string_of_expr e2 ^ ")"
  | Or (e1, e2) -> "(" ^ string_of_expr e1 ^ " Or " ^ string_of_expr e2 ^ ")"
  | Not e1 -> "(Not " ^ string_of_expr e1 ^ ")"
  | Plus (e1, e2) -> "(" ^ string_of_expr e1 ^ " + " ^ string_of_expr e2 ^ ")"
  | Mult (e1, e2) -> "(" ^ string_of_expr e1 ^ " x " ^ string_of_expr e2 ^ ")"
  | Equal (e1, e2) -> "(" ^ string_of_expr e1 ^ " = " ^ string_of_expr e2 ^ ")"
  | If (e1, e2, e3) ->
      "If " ^ string_of_expr e1 ^ " Then " ^ string_of_expr e2 ^ " Else "
      ^ string_of_expr e3

(*
  pretty_trace : expr -> bool -> unit
  in : AST [[e]], stepper flag 
  out : ()
  side effects : Pretty prints execution trace of e. Blocks on keystroke 
  between reductions if stepper is on.
*)
let rec pretty_trace e stepper =
  Printf.printf "%s" (string_of_expr e);
  if stepper then ignore (read_line ()) else ();
  if is_val e then (
    Printf.printf "\n";
    flush stdout)
  else
    match step e with
    | NotReducible ->
        Printf.printf "  (Bad, Stuck Expression)\n";
        flush stdout
    | Step e' ->
        Printf.printf "->\n";
        flush stdout;
        pretty_trace e' stepper

(*
  stepper : expr -> ()
  in : AST [[e]]
  out : ()
  side effects : Pretty prints execution trace of e. Blocks on keystroke 
  between reductions.
*)
let rec stepper e = pretty_trace e true

(*
  tracer : expr -> ()
  in : AST [[e]]
  out : ()
  side effects : Pretty prints execution trace of e. 
*)
let rec tracer e = pretty_trace e false
