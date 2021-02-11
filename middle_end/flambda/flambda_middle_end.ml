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

type middle_end_result = {
  (* CR mshinwell: This next field is redundant *)
  cmx : Flambda_cmx_format.t option;
  unit : Flambda_unit.t;
  all_code : Exported_code.t;
}

let check_invariants unit =
  try Flambda_unit.invariant unit
  with exn -> begin
    Format.eprintf "Unit which failed invariant check:@ %a\n%!"
      Flambda_unit.print unit;
    raise exn
  end

let print_prepared_lambda ppf lam =
  if !Clflags.dump_prepared_lambda then begin
    Format.fprintf ppf "%sAfter Prepare_lambda:%s@ %a@."
      (Flambda_colours.each_file ())
      (Flambda_colours.normal ())
      Printlambda.lambda lam
  end

let print_ilambda ppf (ilam : Ilambda.program) =
  if !Clflags.dump_ilambda then begin
    Format.fprintf ppf
      "\n%sAfter CPS conversion (return continuation %a) \
       (exception continuation %a):%s@ %a@."
      (Flambda_colours.each_file ())
      Continuation.print ilam.return_continuation
      Continuation.print ilam.exn_continuation.exn_handler
      (Flambda_colours.normal ())
      Ilambda.print ilam.expr
  end

let print_ilambda_after_mutable_variable_elimination ppf
      (ilam : Ilambda.program) =
  if !Clflags.dump_ilambda then begin
    Format.fprintf ppf
      "\n%sAfter mutable variable elimination (return continuation %a) \
       (exception continuation %a):%s@ %a@."
      (Flambda_colours.each_file ())
      Continuation.print ilam.return_continuation
      Continuation.print ilam.exn_continuation.exn_handler
      (Flambda_colours.normal ())
      Ilambda.print ilam.expr
  end

let print_rawflambda ppf unit =
  if !Clflags.dump_rawflambda then begin
    Format.fprintf ppf "\n%sAfter closure conversion:%s@ %a@."
      (Flambda_colours.each_file ())
      (Flambda_colours.normal ())
      Flambda_unit.print unit
  end;
  if !Clflags.dump_rawfexpr then begin
    Format.fprintf ppf "\n%sAfter closure conversion:%s@ %a@."
      (Flambda_colours.each_file ())
      (Flambda_colours.normal ())
      Print_fexpr.flambda_unit (unit |> Flambda_to_fexpr.conv)
  end

let print_flambda name ppf unit =
  if !Clflags.dump_flambda then begin
    Format.fprintf ppf "\n%sAfter %s:%s@ %a@."
      (Flambda_colours.each_file ())
      name
      (Flambda_colours.normal ())
      Flambda_unit.print unit
  end;
  if !Clflags.dump_fexpr then begin
    Format.fprintf ppf "\n%sAfter %s:%s@ %a@."
      (Flambda_colours.each_file ())
      name
      (Flambda_colours.normal ())
      Print_fexpr.flambda_unit (unit |> Flambda_to_fexpr.conv)
  end

let output_flexpect ~ml_filename old_unit new_unit =
  if !Clflags.dump_flexpect then begin
    let basename = Filename.chop_suffix ml_filename ".ml" in
    let filename = basename ^ ".flt" in
    let before = old_unit |> Flambda_to_fexpr.conv in
    let after = new_unit |> Flambda_to_fexpr.conv in
    let test : Fexpr.expect_test_spec = { before; after; }
    in
    let out = open_out filename in
    Misc.try_finally ~always:(fun () -> close_out out) (fun () ->
      let ppf = out |> Format.formatter_of_out_channel in
      Print_fexpr.expect_test_spec ppf test;
      Format.pp_print_flush ppf ())
  end

let middle_end0 ppf ~prefixname ~backend ~filename ~module_ident
      ~module_block_size_in_words ~module_initializer =
  Misc.Color.setup !Clflags.color;
  Profile.record_call "flambda.0" (fun () ->
    let prepared_lambda, recursive_static_catches =
      Profile.record_call "prepare_lambda" (fun () ->
        Prepare_lambda.run module_initializer)
    in
    print_prepared_lambda ppf prepared_lambda;
    let ilambda =
      Profile.record_call "cps_conversion" (fun () ->
        Cps_conversion.lambda_to_ilambda prepared_lambda
          ~recursive_static_catches)
    in
    print_ilambda ppf ilambda;
    let ilambda =
      if ilambda.uses_mutable_variables then begin
        let ilambda =
          Profile.record_call "eliminate_mutable_variables" (fun () ->
            Eliminate_mutable_vars.run ilambda)
        in
        print_ilambda_after_mutable_variable_elimination ppf ilambda;
        ilambda
      end else begin
        ilambda
      end
    in
    let flambda =
      Profile.record_call "closure_conversion" (fun () ->
        Closure_conversion.ilambda_to_flambda ~backend ~module_ident
          ~module_block_size_in_words ~filename ilambda)
    in
    print_rawflambda ppf flambda;
    check_invariants flambda;
    let flambda =
      if !Clflags.Flambda.Debug.permute_every_name
      then Flambda_unit.permute_everything flambda
      else flambda
    in
    let round = 0 in
    let new_flambda =
      Profile.record_call ~accumulate:true "simplify"
        (fun () -> Simplify.run ~backend ~round flambda)
    in
    if !Clflags.inlining_report then begin
      let output_prefix = Printf.sprintf "%s.%d" prefixname round in
      Inlining_report.output_then_forget_decisions ~output_prefix
    end;
    print_flambda "simplify" ppf new_flambda.unit;
    output_flexpect ~ml_filename:filename flambda new_flambda.unit;
    new_flambda)

let middle_end ~ppf_dump:ppf ~prefixname ~backend ~filename ~module_ident
      ~module_block_size_in_words ~module_initializer : middle_end_result =
  try
    let simplify_result =
      middle_end0 ppf ~prefixname ~backend ~filename ~module_ident
        ~module_block_size_in_words ~module_initializer
    in
    begin match Sys.getenv "PRINT_SIZES" with
    | exception Not_found -> ()
    | _ ->
      Exported_code.iter simplify_result.all_code (fun id code ->
        match Flambda.Code.size code with
        | Known size -> Format.fprintf Format.std_formatter "%a %a\n" Code_id.print id Flambda.Code_size.print size
        | _ -> assert false);
    end;
    { cmx = simplify_result.cmx;
      unit = simplify_result.unit;
      all_code = simplify_result.all_code;
    }
  with Misc.Fatal_error -> begin
    Format.eprintf "\n%sOriginal backtrace is:%s\n%s\n"
      (Flambda_colours.error ())
      (Flambda_colours.normal ())
      (Printexc.raw_backtrace_to_string (Misc.fatal_error_callstack ()));
    raise Misc.Fatal_error
  end
