(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 2016--2019 OCamlPro SAS                                    *)
(*   Copyright 2016--2019 Jane Street Group LLC                           *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

type simple =
  | Var of Ident.t
  | Const of Lambda.structured_constant

type exn_continuation =
  { exn_handler : Continuation.t;
    extra_args : (simple * Lambda.value_kind) list;
  }

type trap_action =
  | Push of { exn_handler : Continuation.t; }
  | Pop of { exn_handler : Continuation.t; }

type user_visible =
  | User_visible
  | Not_user_visible

type named =
  | Simple of simple
  | Prim of {
      prim : Lambda.primitive;
      args : simple list;
      loc : Lambda.scoped_location;
      exn_continuation : exn_continuation option;
    }

type apply_kind =
  | Function
  | Method of { kind : Lambda.meth_kind; obj : simple; }

type apply = {
  kind : apply_kind;
  func : Ident.t;
  args : simple list;
  continuation : Continuation.t;
  exn_continuation : exn_continuation;
  loc : Lambda.scoped_location;
  tailcall : Lambda.tailcall_attribute;
  inlined : Lambda.inline_attribute;
  specialised : Lambda.specialise_attribute;
}

type switch = {
  numconsts : int;
  consts : (int * Continuation.t * trap_action option * (simple list)) list;
  failaction : (Continuation.t * trap_action option * (simple list)) option;
}

let fprintf = Format.fprintf

let print_simple ppf simple =
  match simple with
  | Var id -> Ident.print ppf id
  | Const cst -> Printlambda.structured_constant ppf cst

let print_named ppf (named : named) =
  match named with
  | Simple (Var id) -> Ident.print ppf id
  | Simple (Const cst) -> Printlambda.structured_constant ppf cst
  | Prim { prim; args; _ } ->
    fprintf ppf "@[<2>(%a %a)@]"
      Printlambda.primitive prim
      (Format.pp_print_list ~pp_sep:Format.pp_print_space print_simple) args


let contains_functions (lam : Lambda.lambda) =
  let rec contains_functions_tail (lam : Lambda.lambda) k =
  match lam with
    | Lvar _  | Lconst _ -> k ()
    | Lfunction _ | Lletrec _ -> true
    | Lassign (_, lam) | Levent (lam, _) | Lifused (_, lam)
      -> contains_functions_tail lam k
    | Lapply { ap_func; ap_args; _}
      -> contains_functions_list (ap_func::ap_args) k
    | Llet (_, _, _, lam1, lam2) | Lstaticcatch (lam1, _, lam2)
    | Ltrywith (lam1, _, lam2) | Lsequence (lam1, lam2)
    | Lwhile (lam1, lam2)
      -> contains_functions_list [lam1; lam2] k
    | Lifthenelse (lam1, lam2, lam3) | Lfor (_, lam1, lam2, _, lam3)
      -> contains_functions_list [lam1; lam2; lam3] k
    | Lprim (_, lams, _) | Lstaticraise (_, lams)
      -> contains_functions_list lams k
    | Lsend (_, lam1, lam2, lams, _)
      -> contains_functions_list (lam1::lam2::lams) k
    | Lswitch (lam1, { sw_consts; sw_blocks; sw_failaction; _ }, _) ->
      let lams1 = List.map snd sw_consts in
      let lams2 = List.map snd sw_blocks in
      let lams = match sw_failaction with
        | None -> lam1::lams1 @ lams2
        | Some lam2 -> lam1::lam2::lams1 @ lams2
      in
      contains_functions_list lams k
    | Lstringswitch (lam1, branches, failaction, _) ->
      let lams = List.map snd branches in
      let lams = match failaction with
        | None -> lam1::lams
        | Some lam2 -> lam1::lam2::lams
      in
      contains_functions_list lams k
  and contains_functions_list lams k =
    match lams with
    | [] -> k ()
    | lam::lams ->
        contains_functions_tail lam
          (fun () -> contains_functions_list lams k)
  in
  contains_functions_tail lam (fun () -> false)
