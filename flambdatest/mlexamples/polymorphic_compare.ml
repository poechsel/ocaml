
let[@inline] foo x y = if compare x y < 0 then x else y

let ref_int x = if compare x 5 < 0 then x else 5
let ref_float x = if compare x 5. < 0 then x else 5.

let test x y = (foo[@inlined]) x y
let test_int x = (foo[@inlined]) x 5
let test_float x = (foo[@inlined]) x 5.


