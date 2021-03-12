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

open Wrapper

val convert_and_bind
   : backend:(module Flambda_backend_intf.S)
  -> Exn_continuation.t option
  -> register_const_string:(string -> Symbol.t)
  -> Lambda.primitive
  -> args:Simple.t list
  -> Debuginfo.t
  -> (Named_with_size.t option -> Expr_with_size.t)
  -> Expr_with_size.t
