(** Define the program *)
type prog =
  | Bool of bool
  | Int of int
  | Add of prog * prog
  | Lt of prog * prog
  | If of prog * prog * prog


(** Perform one reduction step. *)
let rec red : prog -> prog option = function
  | Bool _ | Int _ -> None
  | Add (Int n1 , Int n2) -> Some (Int (n1 + n2))
  | Add (p1 , p2) ->
    (
      match red p1 with
        | Some p1'-> Some (Add (p1', p2))
        | None ->
          match red p2 with
          | Some p2'-> Some (Add (p1 , p2'))
          | None -> None
    )
  | Lt (Int n1 , Int n2) -> Some (Bool (n1 < n2))
  | Lt (p1 , p2) ->
    (
      match red p1 with
      | Some p1'-> Some (Lt (p1', p2))
      | None ->
        match red p2 with
        | Some p2'-> Some (Lt (p1 , p2'))
        | None -> None
    )
  | If (Bool true , p1 , _) -> Some p1
  | If (Bool false , _ , p2) -> Some p2
  | If (p , p1 , p2) ->
    match red p with
      | Some p'-> Some (If (p', p1 , p2))
      | None -> None


(** Define the type of a program. *)
type t = TInt | TBool

let show_t = function
  | TBool -> "TBool"
  | TInt -> "TInt"


(** Define the type error. *)
exception Type_error


(** Infer the type of a program. *)
let rec infer = function
  | Bool _ -> TBool
  | Int _ -> TInt
  | Add (p1 , p2) ->
    check p1 TInt;
    check p2 TInt;
    TInt
  | Lt (p1 , p2) ->
    check p1 TInt;
    check p2 TInt;
    TBool
  | If (p , p1 , p2) ->
    check p TBool;
    let t = infer p1 in
    check p2 t;
    t

(** Check that a program has a given type. *)
and check p t =
  if infer p <> t then raise Type_error

