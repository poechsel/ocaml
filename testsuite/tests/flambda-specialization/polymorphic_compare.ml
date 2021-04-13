(* TEST
 * flambda
 ** native
*)

external minor_words : unit -> (float [@unboxed])
  = "caml_gc_minor_words" "caml_gc_minor_words_unboxed"

let[@inline never] check_no_alloc line f x y expected_res =
  let before = minor_words () in
  let res = (f[@inlined never]) x y in
  let after = minor_words () in
  let diff = after -. before in
  assert (res = expected_res);
  if diff = 0. then
    Format.printf "OK@."
  else
    Format.printf "KO at line %d@." line


(* This test is meant to ensure that flambda adequately specializes
   polymorphic comparison after inlining.
   If the transformation is done, the primitive is known and the
   type can tell that it's below 2 hence Some 42 is a constant.
   So no allocations *)

let[@inline] test_aux_lt x y =
  let res = if Obj.magic (x < y) < 2 then 1 else Sys.opaque_identity 42 in
  Some res

let[@inline never] test_lt (x:float) y = test_aux_lt x y

let () =
  check_no_alloc __LINE__ test_lt 5. 1. (Some 1);
  ()

