(** Test small language. *)
open Typing_as_proving
open Format

(** Useful functions. **)
let print_test_result test = printf "%d" test

(** Test cases. *)
let test1 = id 1

let () = print_test_result test1