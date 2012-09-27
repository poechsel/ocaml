(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 2000 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id: scheduling.ml 12858 2012-08-10 14:45:51Z maranget $ *)

let _ = let module M = Schedgen in () (* to create a dependency *)

(* Scheduling is turned off because the processor schedules dynamically
   much better than what we could do. *)

let fundecl f = f
