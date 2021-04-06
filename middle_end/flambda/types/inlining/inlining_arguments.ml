(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2021 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

type t =
  | Unknown
  | O1
  | O2
  | O3
  | Custom of { max_inlining_depth : int }

let unknown = Unknown

let print ppf = function
  | Unknown -> Format.fprintf ppf "Unknown"
  | O1 -> Format.fprintf ppf "O1"
  | O2 -> Format.fprintf ppf "O2"
  | O3 -> Format.fprintf ppf "O3"
  | Custom t ->
    Format.fprintf ppf
      "@[<hov 1>(max_inlining_depth %d)@]"
      t.max_inlining_depth

let max_inlining_depth = function
  | Unknown -> 0
  | O1 -> 1
  | O2 -> 2
  | O3 -> 3
  | Custom t -> t.max_inlining_depth

let equal t1 t2 =
  max_inlining_depth t1 = max_inlining_depth t2

let compress t =
  match t with
  | O1 | O2 | O3 | Unknown -> t
  | t ->
    if equal t O1 then O1
    else if equal t O2 then O2
    else if equal t O3 then O3
    else t

let refine t1 t2 =
  let max_inlining_depth =
    if (max_inlining_depth t1) <= (max_inlining_depth t2) then
      max_inlining_depth t1
    else
      max_inlining_depth t2
  in
  Custom { max_inlining_depth }

let merge t1 t2 =
  match t1, t2 with
  | Unknown, Unknown -> Unknown
  | Unknown, (O1 | O2 | O3 | Custom _) -> t2
  | (O1 | O2 | O3 | Custom _), Unknown -> t1

  | O1, O1 | O1, (O2 | O3) | (O2 | O3), O1 -> O1
  | O2, O2 | O3, O2 | O2, O3 -> O2
  | O3, O3 -> O3
  | Custom _, _ | _, Custom _ -> refine t1 t2

let create ~round:_ =
  compress (Custom {
    max_inlining_depth = !Clflags.Flambda.Expert.max_inlining_depth
  })
