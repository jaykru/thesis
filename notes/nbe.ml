type ty = Arrow of ty * ty
        | Prod of ty * ty
        | Num

type var = string

let cur : int ref = ref 0

let fresh () : var = Printf.sprintf "%d" !cur

type syn = Lam of var * ty * syn
         | Pair of syn * syn
         | App of syn * syn
         | P1 of syn
         | P2 of syn
         | Var of var
         | Num of int

(* Note: There are no elimination forms in the semantics; they are represented
   as syntax through the SYN embedding. *)
type sem = LAM of (sem -> sem)
         | PAIR of sem * sem
         | SYN of syn

(* reflect is a type-indexed family of functions taking the syntax into the
   semantics *)
let rec reflect (tau : ty) : syn -> sem =
  fun t ->
  match tau with
  | Arrow (a, b) ->
     LAM (fun s -> reflect b (App (t, (reify a s)) ))
  | Prod (a, b) ->
     PAIR ((reflect a (P1 t), (reflect b (P1 t))))
  | Num ->
    SYN t

and reify (tau : ty) : sem -> syn =
  fun t ->
  match (t, tau) with
  | (LAM s, Arrow (a, b)) ->
     let x = fresh () in
     Lam (x, tau, reify b (s (reflect a (Var x))))
  | (PAIR (l, r), Prod(a,b)) ->
     Pair (reify a l, reify b r)
  | (SYN b, Num) ->
     b

type context = (var * ty) list


let nbe (tau : ty) : syn -> syn = fun t -> reflect tau t |> reify tau
