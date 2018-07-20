(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                   Pierre Oechsel, Jane Street Europe                   *)
(*                                                                        *)
(*   Copyright 2018--2018 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

module Unboxing = struct
  type t = {
    unbox_specialised_args : bool;
    unbox_free_vars_of_closures : bool;
    unbox_closures : bool;
    unbox_closures_factor : int;
    remove_unused_arguments : bool;
  }
  type u = t

  let extract args = args

  let get () =
    {
      unbox_specialised_args = !Clflags.unbox_specialised_args;
      unbox_free_vars_of_closures = !Clflags.unbox_free_vars_of_closures;
      unbox_closures = !Clflags.unbox_closures;
      unbox_closures_factor = !Clflags.unbox_closures_factor;
      remove_unused_arguments = !Clflags.remove_unused_arguments;
    }
end


module Inlining = struct
  type u = {
    inline_call_cost : int;
    inline_alloc_cost : int;
    inline_prim_cost : int;
    inline_branch_cost : int;
    inline_indirect_cost : int;
    inline_lifting_benefit : int;
    inline_branch_factor : float;
    inline_max_depth : int;
    inline_max_speculation_depth : int;
    inline_max_unroll : int;
    inline_max_specialise : int;
    inline_threshold : int;
    inline_toplevel_threshold : int;
  }

  type t =
      O1
    | O2
    | O3
    | OClassic
    | Custom of u

  let default_inline_threshold = 10
  let inline_toplevel_multiplier = 16
  let default_inline_toplevel_threshold =
    inline_toplevel_multiplier * default_inline_threshold
  let default_inline_call_cost = 5
  let default_inline_alloc_cost = 7
  let default_inline_prim_cost = 3
  let default_inline_branch_cost = 5
  let default_inline_indirect_cost = 4
  let default_inline_branch_factor = 0.1
  let default_inline_lifting_benefit = 1300
  let default_inline_max_unroll = 0
  let default_inline_max_specialise = 10
  let default_inline_max_depth = 5
  let default_inline_max_speculation_depth = 1
    
  let o1_arguments = {
    inline_call_cost = default_inline_call_cost;
    inline_alloc_cost = default_inline_alloc_cost;
    inline_prim_cost = default_inline_prim_cost;
    inline_branch_cost = default_inline_branch_cost;
    inline_indirect_cost = default_inline_indirect_cost;
    inline_lifting_benefit = default_inline_lifting_benefit;
    inline_branch_factor = default_inline_branch_factor;
    inline_max_depth = default_inline_max_depth;
    inline_max_speculation_depth = default_inline_max_speculation_depth;
    inline_max_unroll = default_inline_max_unroll;
    inline_max_specialise = default_inline_max_specialise;
    inline_threshold = default_inline_threshold;
    inline_toplevel_threshold = default_inline_toplevel_threshold;
  }

  let classic_arguments = {
    inline_call_cost = default_inline_call_cost;
    inline_alloc_cost = default_inline_alloc_cost;
    inline_prim_cost = default_inline_prim_cost;
    inline_branch_cost = default_inline_branch_cost;
    inline_indirect_cost = default_inline_indirect_cost;
    inline_lifting_benefit = default_inline_lifting_benefit;
    inline_branch_factor = default_inline_branch_factor;
    inline_max_depth = default_inline_max_depth;
    inline_max_speculation_depth = default_inline_max_speculation_depth;
    inline_max_unroll = default_inline_max_unroll;
    inline_max_specialise = default_inline_max_specialise;
    (* [inline_threshold] matches the current compiler's default.
       Note that this particular fraction can be expressed exactly in
       floating point. *)
    inline_threshold = 10;
    (* [inline_toplevel_threshold] is not used in classic mode. *)
    inline_toplevel_threshold = 1;
  }

  let o2_arguments = {
    inline_call_cost = 2 * default_inline_call_cost;
    inline_alloc_cost = 2 * default_inline_alloc_cost;
    inline_prim_cost = 2 * default_inline_prim_cost;
    inline_branch_cost = 2 * default_inline_branch_cost;
    inline_indirect_cost = 2 * default_inline_indirect_cost;
    inline_lifting_benefit = default_inline_lifting_benefit;
    inline_branch_factor = default_inline_branch_factor;
    inline_max_depth = 2 * default_inline_max_depth;
    inline_max_speculation_depth = 2 * default_inline_max_speculation_depth;
    inline_max_unroll = default_inline_max_unroll;
    inline_max_specialise = 2 * default_inline_max_specialise;
    inline_threshold = 25;
    inline_toplevel_threshold = 25 * inline_toplevel_multiplier;
  }

  let o3_arguments = {
    inline_call_cost = 3 * default_inline_call_cost;
    inline_alloc_cost = 3 * default_inline_alloc_cost;
    inline_prim_cost = 3 * default_inline_prim_cost;
    inline_branch_cost = 3 * default_inline_branch_cost;
    inline_indirect_cost = 3 * default_inline_indirect_cost;
    inline_lifting_benefit = default_inline_lifting_benefit;
    inline_branch_factor = 0.;
    inline_max_depth = 3 * default_inline_max_depth;
    inline_max_speculation_depth = 3 * default_inline_max_speculation_depth;
    inline_max_unroll = 1;
    inline_max_specialise = 3 * default_inline_max_specialise;
    inline_threshold = 50;
    inline_toplevel_threshold = 50 * inline_toplevel_multiplier;
  }

  let inline_call_cost args =
    match args with
    | O1 -> o1_arguments.inline_call_cost
    | O2 -> o2_arguments.inline_call_cost
    | O3 -> o3_arguments.inline_call_cost
    | OClassic -> classic_arguments.inline_call_cost
    | Custom t -> t.inline_call_cost

  let inline_alloc_cost args =
    match args with
    | O1 -> o1_arguments.inline_alloc_cost
    | O2 -> o2_arguments.inline_alloc_cost
    | O3 -> o3_arguments.inline_alloc_cost
    | OClassic -> classic_arguments.inline_alloc_cost
    | Custom t -> t.inline_alloc_cost
                    
  let inline_prim_cost args =
    match args with
    | O1 -> o1_arguments.inline_prim_cost
    | O2 -> o2_arguments.inline_prim_cost
    | O3 -> o3_arguments.inline_prim_cost
    | OClassic -> classic_arguments.inline_prim_cost
    | Custom t -> t.inline_prim_cost
                    
  let inline_branch_cost args =
    match args with
    | O1 -> o1_arguments.inline_branch_cost
    | O2 -> o2_arguments.inline_branch_cost
    | O3 -> o3_arguments.inline_branch_cost
    | OClassic -> classic_arguments.inline_branch_cost
    | Custom t -> t.inline_branch_cost
                    
  let inline_indirect_cost args =
    match args with
    | O1 -> o1_arguments.inline_indirect_cost
    | O2 -> o2_arguments.inline_indirect_cost
    | O3 -> o3_arguments.inline_indirect_cost
    | OClassic -> classic_arguments.inline_indirect_cost
    | Custom t -> t.inline_indirect_cost
                    
  let inline_lifting_benefit args =
    match args with
    | O1 -> o1_arguments.inline_lifting_benefit
    | O2 -> o2_arguments.inline_lifting_benefit
    | O3 -> o3_arguments.inline_lifting_benefit
    | OClassic -> classic_arguments.inline_lifting_benefit
    | Custom t -> t.inline_lifting_benefit
                    
  let inline_branch_factor args =
    match args with
    | O1 -> o1_arguments.inline_branch_factor
    | O2 -> o2_arguments.inline_branch_factor
    | O3 -> o3_arguments.inline_branch_factor
    | OClassic -> classic_arguments.inline_branch_factor
    | Custom t -> t.inline_branch_factor
                    
  let inline_max_depth args =
    match args with
    | O1 -> o1_arguments.inline_max_depth
    | O2 -> o2_arguments.inline_max_depth
    | O3 -> o3_arguments.inline_max_depth
    | OClassic -> classic_arguments.inline_max_depth
    | Custom t -> t.inline_max_depth

  let inline_max_speculation_depth args =
    match args with
    | O1 -> o1_arguments.inline_max_speculation_depth
    | O2 -> o2_arguments.inline_max_speculation_depth
    | O3 -> o3_arguments.inline_max_speculation_depth
    | OClassic -> classic_arguments.inline_max_speculation_depth
    | Custom t -> t.inline_max_speculation_depth

  let inline_max_unroll args =
    match args with
    | O1 -> o1_arguments.inline_max_unroll
    | O2 -> o2_arguments.inline_max_unroll
    | O3 -> o3_arguments.inline_max_unroll
    | OClassic -> classic_arguments.inline_max_unroll
    | Custom t -> t.inline_max_unroll
                    
  let inline_max_specialise args =
    match args with
    | O1 -> o1_arguments.inline_max_specialise
    | O2 -> o2_arguments.inline_max_specialise
    | O3 -> o3_arguments.inline_max_specialise
    | OClassic -> classic_arguments.inline_max_specialise
    | Custom t -> t.inline_max_specialise
                    
  let inline_threshold args =
    match args with
    | O1 -> o1_arguments.inline_threshold
    | O2 -> o2_arguments.inline_threshold
    | O3 -> o3_arguments.inline_threshold
    | OClassic -> classic_arguments.inline_threshold
    | Custom t -> t.inline_threshold
                    
  let inline_toplevel_threshold args =
    match args with
    | O1 -> o1_arguments.inline_toplevel_threshold
    | O2 -> o2_arguments.inline_toplevel_threshold
    | O3 -> o3_arguments.inline_toplevel_threshold
    | OClassic -> classic_arguments.inline_toplevel_threshold
    | Custom t -> t.inline_toplevel_threshold

  let cost ~round flag fn =
    let v = Clflags.Int_arg_helper.get ~key:round flag in
    match (v : Clflags.inlining_argument) with
    | Clflags.O1 -> fn o1_arguments
    | Clflags.O2 -> fn o2_arguments
    | Clflags.O3 -> fn o3_arguments
    | Clflags.OClassic -> fn classic_arguments
    | Clflags.Int x -> x
    | _ -> assert false
      
  let cost_f ~round flag fn =
    let v = Clflags.Float_arg_helper.get ~key:round flag in
    match (v : Clflags.inlining_argument) with
    | Clflags.O1 -> fn o1_arguments
    | Clflags.O2 -> fn o2_arguments
    | Clflags.O3 -> fn o3_arguments
    | Clflags.OClassic -> fn classic_arguments
    | Clflags.Float x -> x
    | _ -> assert false

  let compress theory = 
    if theory = o1_arguments then
      O1
    else if theory = o2_arguments then
      O2
    else if theory = o3_arguments then
      O3
    else if theory = classic_arguments then
      OClassic
    else
      Custom theory

  let get round =
    let theory = {
      inline_call_cost =
        cost !Clflags.inline_call_cost ~round
          (fun x -> x.inline_call_cost);
      inline_alloc_cost =
        cost !Clflags.inline_alloc_cost ~round
          (fun x -> x.inline_alloc_cost);
      inline_prim_cost =
        cost !Clflags.inline_prim_cost ~round
          (fun x -> x.inline_prim_cost);
      inline_branch_cost =
        cost !Clflags.inline_branch_cost ~round
          (fun x -> x.inline_branch_cost);
      inline_indirect_cost =
        cost !Clflags.inline_indirect_cost ~round
          (fun x -> x.inline_indirect_cost);
      inline_lifting_benefit =
        cost !Clflags.inline_lifting_benefit ~round
          (fun x -> x.inline_lifting_benefit);
      inline_branch_factor =
        cost_f !Clflags.inline_branch_factor ~round
          (fun x -> x.inline_branch_factor);
      inline_max_depth =
        cost !Clflags.inline_max_depth ~round
          (fun x -> x.inline_max_depth);
      inline_max_speculation_depth =
        cost !Clflags.inline_max_speculation_depth ~round
          (fun x -> x.inline_max_speculation_depth);
      inline_max_unroll =
        cost !Clflags.inline_max_unroll ~round
          (fun x -> x.inline_max_unroll);
      inline_max_specialise =
        cost !Clflags.inline_max_specialise ~round
          (fun x -> x.inline_max_specialise);
      inline_threshold =
        cost !Clflags.inline_threshold ~round
          (fun x -> x.inline_threshold);
      inline_toplevel_threshold =
        cost !Clflags.inline_toplevel_threshold ~round
          (fun x -> x.inline_toplevel_threshold);
    } in
    compress theory


  let get_max () =
    let round = Clflags.rounds () - 1 in
    get round

  let merge args1 args2 =
    match args1, args2 with
    | Custom _, _ | _, Custom _ ->
        {
          inline_call_cost =
            min (inline_call_cost args1) (inline_call_cost args2);
          inline_alloc_cost =
            min (inline_alloc_cost args1) (inline_alloc_cost args2);
          inline_prim_cost =
            min (inline_prim_cost args1) (inline_prim_cost args2);
          inline_branch_cost =
            min (inline_branch_cost args1) (inline_branch_cost args2);
          inline_indirect_cost =
            min (inline_indirect_cost args1) (inline_indirect_cost args2);
          inline_lifting_benefit =
            min (inline_lifting_benefit args1) (inline_lifting_benefit args2);
          inline_branch_factor =
            min (inline_branch_factor args1) (inline_branch_factor args2);
          inline_max_depth =
            min (inline_max_depth args1) (inline_max_depth args2);
          inline_max_speculation_depth =
            min (inline_max_speculation_depth args1) (inline_max_speculation_depth args2);
          inline_max_unroll =
            min (inline_max_unroll args1) (inline_max_unroll args2);
          inline_max_specialise =
            min (inline_max_specialise args1) (inline_max_specialise args2);
          inline_threshold =
            min (inline_threshold args1) (inline_threshold args2);
          inline_toplevel_threshold =
            min (inline_toplevel_threshold args1) (inline_toplevel_threshold args2);
        }
        |> compress
    | OClassic, _ | _, OClassic -> OClassic
    | O1, _ | _, O1 -> O1
    | O2, _ | _, O2 -> O2
    | _ -> O3

    let ensure_integrity () =
    let rec iter_through_round round previous =
      if round < Clflags.rounds () then begin
        let attrs_int =
          [("inline_call_cost",
            inline_call_cost,
            (fun r x ->
               Clflags.inline_call_cost :=
                 Clflags.Int_arg_helper.add_user_override r (Clflags.Int x)
                   !Clflags.inline_call_cost));
           ("inline_alloc_cost",
            inline_alloc_cost,
            (fun r x ->
               Clflags.inline_alloc_cost :=
                 Clflags.Int_arg_helper.add_user_override r (Clflags.Int x)
                   !Clflags.inline_alloc_cost));
           ("inline_prim_cost",
            inline_prim_cost,
            (fun r x ->
               Clflags.inline_prim_cost :=
                 Clflags.Int_arg_helper.add_user_override r (Clflags.Int x)
                   !Clflags.inline_prim_cost));
           ("inline_branch_cost",
            inline_branch_cost,
            (fun r x ->
               Clflags.inline_branch_cost :=
                 Clflags.Int_arg_helper.add_user_override r (Clflags.Int x)
                   !Clflags.inline_branch_cost));
           ("inline_indirect_cost",
            inline_indirect_cost,
            (fun r x ->
               Clflags.inline_indirect_cost :=
                 Clflags.Int_arg_helper.add_user_override r (Clflags.Int x)
                   !Clflags.inline_indirect_cost));
           ("inline_lifting_benefit",
            inline_lifting_benefit,
            (fun r x ->
               Clflags.inline_lifting_benefit :=
                 Clflags.Int_arg_helper.add_user_override r (Clflags.Int x)
                   !Clflags.inline_lifting_benefit));
           ("inline_max_depth",
            inline_max_depth,
            (fun r x ->
               Clflags.inline_max_depth :=
                 Clflags.Int_arg_helper.add_user_override r (Clflags.Int x)
                   !Clflags.inline_max_depth));
           ("inline_max_speculation_depth",
            inline_max_speculation_depth,
            (fun r x ->
               Clflags.inline_max_speculation_depth :=
                 Clflags.Int_arg_helper.add_user_override r (Clflags.Int x)
                   !Clflags.inline_max_speculation_depth));
           ("inline_max_unroll",
            inline_max_unroll,
            (fun r x ->
               Clflags.inline_max_unroll :=
                 Clflags.Int_arg_helper.add_user_override r (Clflags.Int x)
                   !Clflags.inline_max_unroll));
           ("inline_max_specialise",
            inline_max_specialise,
            (fun r x ->
               Clflags.inline_max_specialise :=
                 Clflags.Int_arg_helper.add_user_override r (Clflags.Int x)
                   !Clflags.inline_max_specialise));
           ("inline_toplevel_threshold",
            inline_toplevel_threshold,
            (fun r x ->
               Clflags.inline_toplevel_threshold :=
                 Clflags.Int_arg_helper.add_user_override r (Clflags.Int x)
                   !Clflags.inline_toplevel_threshold));
          ("inline_threshold",
            inline_threshold,
            (fun r x ->
               Clflags.inline_threshold :=
                 Clflags.Int_arg_helper.add_user_override r (Clflags.Int x)
                   !Clflags.inline_threshold));]
        in
        let attrs_float =
           [("inline_branch_factor",
            inline_branch_factor,
            (fun r x ->
               Clflags.inline_branch_factor :=
                 Clflags.Float_arg_helper.add_user_override r (Clflags.Float x)
                   !Clflags.inline_branch_factor));]
        in
        let current = get round in
        let action format (label, proj, updater) =
          let non_incr = label = "inline_branch_factor" in
          if (non_incr && proj previous < proj current)
           || (not non_incr && proj previous > proj current) then begin
            let monotony = if non_incr then "decreasing" else "increasing" in
            let message = "Argument " ^ label ^ " is " ^ monotony ^ " between round " ^
                          string_of_int (round - 1) ^ " (=" ^ (format (proj previous)) ^ ") and " ^
                          string_of_int round ^ " (=" ^ (format (proj current)) ^ ")"
            in
            Location.prerr_warning ({loc_start = Lexing.dummy_pos;
                                     loc_end = Lexing.dummy_pos;
                                     loc_ghost = true})
              (Warnings.Non_monotonic_inlining_arguments message);
            updater round (proj previous)
          end
        in
        List.iter (action string_of_int) attrs_int;
        List.iter (action string_of_float) attrs_float;
        let current = get round in
        iter_through_round (round+1) current
      end
    in iter_through_round 1 (get 0)
end
