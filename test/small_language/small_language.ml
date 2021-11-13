(** Test small language. *)
open Small_language

(** Useful functions. *)
let print_test_result test = print_endline (show_t (infer test))

(** Test cases. *)
let test1 = If (Lt (Int 3, Int 5), Int 5, Int 1)
let () = print_test_result test1

(* let test2 = If (Int 1, Int 5, Int 8)
let () = print_test_result test2 *)

(* let test3 = If (Lt (Int 0, Int 1), Bool true, Int 8)
let () = print_test_result test3 *)

