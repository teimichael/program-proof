(*** Arrow as implication ***)
(* A => A *)
let id : 'a -> 'a = fun x -> x

(* A => B => A *)
let k : 'a -> 'b -> 'a = fun x _ -> x

(* (A => B) => (B => C) => (A => C) *)
let comp : ('a -> 'b) -> ('b -> 'c) -> ('a -> 'c) = fun f g x -> g (f x)

(* (A => B => C) => (A => B) => (A => C) *)
let s : ('a -> 'b -> 'c) -> ('a -> 'b) -> ('a -> 'c) = fun f g x -> f x (g x)

(*** Other connectives ***)
(** Conjunction **)
(* A and B => A *)
let proj1 : ('a * 'b) -> 'a = fun (a, _) -> a
(* Commutativitiy of conjunction*)
let comm_con : ('a * 'b) -> ('b * 'a) = fun (a, b) -> b, a

(** Truth **)
(* A => T *)
let unit_intro : 'a -> unit = fun _ -> ()

(** Falsity **)
(* F => A *)
type empty = |
let empty_elim : empty -> 'a = fun x -> match x with _ -> .

(** Negation **)
(* (A => B) => (not B => not A) *)
let contr : ('a -> 'b) -> (('b -> empty) -> ('a -> empty)) = fun f g a -> g (f a)

(* A => not not A *)
let nni : 'a -> (('a -> empty) -> empty) = fun a f -> f a

(** Disjunction **)
type ('a, 'b) coprod = Left of 'a | Right of 'b

(* Commutativity of disjunction A or B => B or A *)
let comm_dis : ('a, 'b) coprod -> ('b, 'a) coprod = fun x ->
  match x with
  | Left a -> Right a
  | Right b -> Left b

(* Distributivity A and (B or C) => (A and B) or (A and C) *)
let dist : ('a * ('b, 'c) coprod) -> ('a * 'b, 'a * 'c) coprod = fun (a, x) ->
  match x with
  | Left b -> Left (a, b)
  | Right c -> Right (a, c)

(* de Morgan (not A or B) => (A => B) *)
let de_Morgan : ('a -> empty, 'b) coprod -> ('a -> 'b) = fun x a ->
  match x with
  | Left f -> empty_elim (f a)
  | Right b -> b

(*** Advanced features ***)
let absurd : 'a -> 'b = fun _ -> raise Not_found

(* A => B *)
(* let rec absurd : 'a -> 'b = fun x -> absurd x *)

(* Prove F *)
(* let fake : empty = absurd () *)
