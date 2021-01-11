(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2019 OCamlPro SAS                                    *)
(*   Copyright 2014--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

type t = {depth:int}

let increment_depth t = { depth = t.depth + 1 } 

let default = { depth = 0 }

let create ~depth = { depth }

let valid t = t.depth >= 0

let print ppf t = Format.fprintf ppf "@[<hov 1>(depth@ %d)@]" t.depth

let exists t = t.depth > 0

let is_depth_exceeded t = t.depth >= !Clflags.Flambda.Expert.max_inlining_depth

let merge t1 t2 = { depth = t1.depth + t2.depth }

let equal t1 t2 = t1.depth = t2.depth
