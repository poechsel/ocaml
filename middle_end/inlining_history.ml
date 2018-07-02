(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2016 OCamlPro SAS                                    *)
(*   Copyright 2014--2016 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

type t = node list

and node =
  | Module of string * Debuginfo.t
  | Closure of Closure_id.t * Debuginfo.t
  | Call of Closure_id.t * Lambda.DebugNames.t * Debuginfo.t * t option
  | Inlined
  | Specialised of Closure_id.Set.t

let create () = []

(* beware, the following are more to check equality than true
   order *)
let rec compare_node a b =
  match a, b with
  | Inlined, Inlined ->
    0
  | Module (a, b), Module(a', b') ->
    if a = a' then 0
    else Debuginfo.compare b b'
  | Closure(a, b), Closure(a', b') ->
    let c = Closure_id.compare a a' in
    if c <> 0 then c else
      Debuginfo.compare b b'
  | Call(a, _, b, p), Call(a', _, b', p') ->
    let c = Closure_id.compare a a' in
    if c <> 0 then c else
      let c = Debuginfo.compare b b' in
      if c <> 0 then c else begin
        match p, p' with
        | None, None -> 0
        | Some l, Some l' ->
          compare l l'
        | _ -> -1
      end
  | Specialised(a), Specialised(a') ->
    Closure_id.Set.compare a a'
  | _ -> -1

and compare l l' =
  let c = List.compare_lengths l l' in
  if c <> 0 then c else
    List.fold_left2 (fun p e e' ->
      if p <> 0 then p
      else compare_node e e')
      0 l l'

let strip_history hist =
  List.map (function | Call(a, c, b, _) -> Call (a, c, b, None)
                     | x -> x) hist

let print_node ppf x =
  match x with
  | Inlined ->
    Format.fprintf ppf "inlined "
  | Call (c, _, _, _) ->
    Format.fprintf ppf "call(%s) " (Closure_id.unique_name c)
  | Closure (c, _) ->
    Format.fprintf ppf "closure(%s) " (Closure_id.unique_name c)
  | Module (c, _) ->
    Format.fprintf ppf "module(%s) " c
  | _ ->
    Format.fprintf ppf "specialise "

let print ppf l =
  List.iter (print_node ppf) l

let add a b =
  (* order is important. If b= [1; 2] and a = [3;4;5],
     we want the result to be [1;2;3;4;5] *)
  a @ b

let note_entering_closure t ~closure_id ~dbg =
  if not !Clflags.inlining_report then t
  else
    match t with
    | [] | (Closure _ | Inlined | Specialised _ | Module _)  :: _->
      (Closure (closure_id, dbg)) :: t
    | (Call _) :: _ ->
      Misc.fatal_errorf "note_entering_closure: unexpected Call node"

(* CR-someday lwhite: since calls do not have a unique id it is possible
   some calls will end up sharing nodes. *)
let note_entering_call t ~closure_id ~dbg_name ~dbg
      ~absolute_inlining_history =
  (Call (closure_id, dbg_name, dbg, absolute_inlining_history)) :: t

let note_entering_inlined t =
  if not !Clflags.inlining_report then t
  else
    match t with
    | [] | (Closure _ | Inlined | Specialised _ | Module _) :: _->
      Misc.fatal_errorf "anote_entering_inlined: missing Call node"
    | (Call _) :: _ -> Inlined :: t

let note_entering_specialised t ~closure_ids =
  if not !Clflags.inlining_report then t
  else
    match t with
    | [] | (Closure _ | Inlined | Specialised _ | Module _) :: _ ->
      Misc.fatal_errorf "bnote_entering_specialised: missing Call node"
    | (Call _) :: _ -> Specialised closure_ids :: t
