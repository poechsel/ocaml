(* TEST
   flags = " -w A "
   * expect
*)

module type U = sig end
[%%expect {|
module type U = sig end
|}]

module M : sig
  module F2 (_ : U) : U
end = struct
  module X = struct
    let x = 13
  end

  module F1 (_ : U) = X
  module F2 (M : U) = F1 (M)
end
[%%expect {|
Line 5, characters 8-9:
5 |     let x = 13
            ^
<<<<<<< HEAD
Warning 32: unused value x.
=======
Warning 32 [unused-value-declaration]: unused value x.
>>>>>>> ocaml/4.12
module M : sig module F2 : U -> U end
|}]

module N : sig
  module F2 (_ : U) : U
end = struct
  module X = struct
    let x = 13
  end

  module F1 (_ : U) = X
  module F2 (_ : U) = F1 (struct end)
end
[%%expect {|
Line 5, characters 8-9:
5 |     let x = 13
            ^
<<<<<<< HEAD
Warning 32: unused value x.
=======
Warning 32 [unused-value-declaration]: unused value x.
>>>>>>> ocaml/4.12
module N : sig module F2 : U -> U end
|}]


module F (X : sig type t type s end) = struct type t = X.t end
[%%expect {|
Line 1, characters 25-31:
1 | module F (X : sig type t type s end) = struct type t = X.t end
                             ^^^^^^
<<<<<<< HEAD
Warning 34: unused type s.
=======
Warning 34 [unused-type-declaration]: unused type s.
>>>>>>> ocaml/4.12
module F : functor (X : sig type t type s end) -> sig type t = X.t end
|}]
