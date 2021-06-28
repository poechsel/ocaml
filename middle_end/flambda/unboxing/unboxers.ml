(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2020 OCamlPro SAS                                    *)
(*   Copyright 2014--2020 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-30-40-41-42"]

open! Simplify_import

type number_decider = {
  param_name : string;
  kind : K.Naked_number_kind.t;
  prove_is_a_boxed_number : TE.t -> T.t -> unit T.proof_allowing_kind_mismatch;
}

type unboxer = {
  var_name : string;
  invalid_const : Const.t;
  unboxing_prim : Simple.t -> P.t;
  prove_simple : TE.t -> min_name_mode:Name_mode.t -> T.t -> Simple.t T.proof;
}

module type Number_S = sig
  val decider : number_decider
  val unboxing_prim : Simple.t -> P.t
  val unboxer : unboxer
end

module Immediate = struct
  let decider = {
    param_name = "naked_immediate";
    kind = K.Naked_number_kind.Naked_immediate;
    prove_is_a_boxed_number = T.prove_is_a_tagged_immediate;
  }

  let unboxing_prim simple =
    P.(Unary (Unbox_number Untagged_immediate, simple))

  let unboxer = {
    var_name = "naked_immediate";
    invalid_const =
      Const.naked_immediate (Targetint_31_63.int (Targetint_31_63.Imm.of_int 0xabcd));
    unboxing_prim;
    prove_simple = T.prove_is_always_tagging_of_simple;
  }
end

module Float = struct
  let decider = {
    param_name = "unboxed_float";
    kind = K.Naked_number_kind.Naked_float;
    prove_is_a_boxed_number = T.prove_is_a_boxed_float;
  }

  let unboxing_prim simple =
    P.(Unary (Unbox_number Naked_float, simple))

  let unboxer = {
    var_name = "unboxed_float";
    invalid_const = Const.naked_float Numbers.Float_by_bit_pattern.zero;
    unboxing_prim;
    prove_simple = T.prove_boxed_float_containing_simple;
  }
end

module Int32 = struct
  let decider = {
    param_name = "unboxed_int32";
    kind = K.Naked_number_kind.Naked_int32;
    prove_is_a_boxed_number = T.prove_is_a_boxed_int32;
  }

  let unboxing_prim simple =
    P.(Unary (Unbox_number Naked_int32, simple))

  let unboxer = {
    var_name = "unboxed_int32";
    invalid_const = Const.naked_int32 Int32.(div 0xabcd0l 2l);
    unboxing_prim;
    prove_simple = T.prove_boxed_int32_containing_simple;
  }
end

module Int64 = struct
  let decider = {
    param_name = "unboxed_int64";
    kind = K.Naked_number_kind.Naked_int64;
    prove_is_a_boxed_number = T.prove_is_a_boxed_int64;
  }

  let unboxing_prim simple =
    P.(Unary (Unbox_number Naked_int64, simple))

  let unboxer = {
    var_name = "unboxed_int64";
    invalid_const = Const.naked_int64 Int64.(div 0xdcba0L 2L);
    unboxing_prim;
    prove_simple = T.prove_boxed_int64_containing_simple;
  }
end

module Nativeint = struct
  let decider = {
    param_name = "unboxed_nativeint";
    kind = K.Naked_number_kind.Naked_nativeint;
    prove_is_a_boxed_number = T.prove_is_a_boxed_nativeint;
  }

  let unboxing_prim simple =
    P.(Unary (Unbox_number Naked_nativeint, simple))

  let unboxer = {
    var_name = "unboxed_nativeint";
    invalid_const = Const.naked_nativeint Targetint.zero;
    unboxing_prim;
    prove_simple = T.prove_boxed_nativeint_containing_simple;
  }
end

module Field = struct
  let unboxing_prim bak ~block ~index =
    let field_const = Simple.const (Const.tagged_immediate index) in
    P.Binary (Block_load (bak, Immutable), block, field_const)

  let unboxer ~invalid_const bak ~index = {
    var_name = "field_at_use";
    invalid_const;
    unboxing_prim = (fun block -> unboxing_prim bak ~block ~index);
    prove_simple = (fun tenv ~min_name_mode t ->
      T.prove_block_field_simple tenv ~min_name_mode t index);
  }
end

module Closure_field = struct
  let unboxing_prim closure_id ~closure var =
    P.Unary (Project_var { project_from = closure_id; var }, closure)

  let unboxer closure_id var = {
    var_name = "closure_field_at_use";
    invalid_const = Const.const_zero;
    unboxing_prim = (fun closure -> unboxing_prim closure_id ~closure var);
    prove_simple = (fun tenv ~min_name_mode t ->
      T.prove_project_var_simple tenv ~min_name_mode t var);
  }
end