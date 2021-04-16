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
let[@inline] test_aux_le x y =
  let res = if Obj.magic (x <= y) < 2 then 1 else Sys.opaque_identity 42 in
  Some res
let[@inline] test_aux_gt x y =
  let res = if Obj.magic (x > y) < 2 then 1 else Sys.opaque_identity 42 in
  Some res
let[@inline] test_aux_ge x y =
  let res = if Obj.magic (x >= y) < 2 then 1 else Sys.opaque_identity 42 in
  Some res
let[@inline] test_aux_eq x y =
  let res = if Obj.magic (x = y) < 2 then 1 else Sys.opaque_identity 42 in
  Some res
let[@inline] test_aux_ne x y =
  let res = if Obj.magic (x != y) < 2 then 1 else Sys.opaque_identity 42 in
  Some res
let[@inline] test_aux_cmp x y =
  let res = if Obj.magic (compare x y) < 2 then 1 else Sys.opaque_identity 42 in
  Some res

let[@inline never] test_lt (x:float) y = test_aux_lt x y
let[@inline never] test_le (x:int32) y = test_aux_le x y
let[@inline never] test_gt (x:int64) y = test_aux_gt x y
let[@inline never] test_ge (x:float) y = test_aux_ge x y
let[@inline never] test_eq (x:float) y = test_aux_eq x y
let[@inline never] test_ne (x:float) y = test_aux_ne x y
let[@inline never] test_cmp (x:float) y = test_aux_cmp x y
let[@inline never] test_fail x y = test_aux_cmp x y

let () =
  check_no_alloc __LINE__ test_lt 5. 1. (Some 1);
  check_no_alloc __LINE__ test_le 5l 1l (Some 1);
  check_no_alloc __LINE__ test_gt 5L 1L (Some 1);
  check_no_alloc __LINE__ test_ge 5. 1. (Some 1);
  check_no_alloc __LINE__ test_eq 5. 1. (Some 1);
  check_no_alloc __LINE__ test_ne 5. 1. (Some 1);
  check_no_alloc __LINE__ test_cmp 5. 1. (Some 1);
  check_no_alloc __LINE__ test_fail () () (Some 1);
  ()

