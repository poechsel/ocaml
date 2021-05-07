(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                   Mark Shinwell, Jane Street Europe                    *)
(*                                                                        *)
(*   Copyright 2019 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type t =
  | Id
  | Non_id of {
      from_depth : int;
      to_depth : int;
    }

let id = Id

let change_depth ~from:from_depth ~to_:to_depth =
  if from_depth = to_depth then Id else Non_id { from_depth; to_depth }

let is_id = function
  | Id -> true
  | Non_id _ -> false

let inverse = function
  | Id -> Id
  | Non_id { from_depth; to_depth } ->
    Non_id { from_depth = to_depth; to_depth = from_depth }

let print ppf = function
  | Id ->
    Format.fprintf ppf "@<0>%sid@<0>%s"
      (Flambda_colours.elide ())
      (Flambda_colours.normal ())
  | Non_id { from_depth; to_depth; } ->
    Format.fprintf ppf "@<0>%s@[<hov 1>(depth@ %d ->@ %d)@]@<0>%s"
      (Flambda_colours.coercion ())
      from_depth to_depth
      (Flambda_colours.normal ())

let compose t1 ~then_:t2 =
  match t1, t2 with
  | Id, _ -> Some t2
  | _, Id -> Some t1
  | Non_id { from_depth = from_depth1; to_depth = to_depth1 },
    Non_id { from_depth = from_depth2; to_depth = to_depth2 } ->
    if to_depth1 = from_depth2 then
      Some (change_depth ~from:from_depth1 ~to_:to_depth2)
    else
      None

let compose_exn t1 ~then_:t2 =
  match compose t1 ~then_:t2 with
  | Some t -> t
  | None ->
    Misc.fatal_errorf "Invalid composition: %a@ >>@ %a" print t1 print t2

let equal t1 t2 =
  match t1, t2 with
  | Id, Id -> true
  | Non_id { from_depth = from_depth1; to_depth = to_depth1 },
    Non_id { from_depth = from_depth2; to_depth = to_depth2 } ->
    from_depth1 = from_depth2 && to_depth1 = to_depth2
  | _, _ -> false

let hash = Hashtbl.hash

let apply_to_rec_info _ rec_info = rec_info
