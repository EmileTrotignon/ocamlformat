(**************************************************************************)
(*                                                                        *)
(*                              OCamlFormat                               *)
(*                                                                        *)
(*            Copyright (c) Facebook, Inc. and its affiliates.            *)
(*                                                                        *)
(*      This source code is licensed under the MIT license found in       *)
(*      the LICENSE file in the root directory of this source tree.       *)
(*                                                                        *)
(**************************************************************************)

(** OCamlFormat *)

open Ocamlformat

let () = Caml.at_exit (Format.pp_print_flush Format.err_formatter)

let () = Caml.at_exit (Format_.pp_print_flush Format_.err_formatter)

let format ?output_file ~kind ~input_name ~source (conf : Conf.t) =
  if conf.opr_opts.disable.v then Ok source
  else
    Translation_unit.parse_and_format kind ?output_file ~input_name ~source
      conf

(** Do not escape from [build_config] *)
exception Conf_error of string

let failwith_user_errors ~from errors =
  let open Format in
  let pp_error pp e = pp_print_string pp (Config_option.Error.to_string e) in
  let pp_errors = pp_print_list ~pp_sep:pp_print_newline pp_error in
  let msg = asprintf "Error while parsing %s:@ %a" from pp_errors errors in
  raise (Conf_error msg)

let read_config_file ?version_check ?disable_conf_attrs conf filename =
  let open Conf in
  try
    In_channel.with_file filename ~f:(fun ic ->
        let lines =
          In_channel.input_lines ic
          |> Migrate_ast.Location.of_lines ~filename
        in
        let c, errors =
          List.fold_left lines ~init:(conf, [])
            ~f:(fun (conf, errors) {txt= line; loc} ->
              let from = `File loc in
              match
                parse_line conf ?version_check ?disable_conf_attrs ~from line
              with
              | Ok conf -> (conf, errors)
              | Error e -> (conf, e :: errors) )
        in
        match List.rev errors with
        | [] -> c
        | l -> failwith_user_errors ~from:filename l )
  with Sys_error _ -> conf

let read_config_files filenames =
  let read_config_file =
    read_config_file ~version_check:false ~disable_conf_attrs:false
  in
  List.fold filenames ~init:Conf.default ~f:read_config_file

let build_config filenames =
  try
    let conf, warn_now =
      Conf.collect_warnings (fun () -> read_config_files filenames)
    in
    if not conf.opr_opts.quiet.v then warn_now () ;
    Ok conf
  with Conf_error msg -> Error msg

let print_error (conf : Conf.t) e =
  Translation_unit.Error.print Format.err_formatter
    ~debug:conf.opr_opts.debug.v ~quiet:conf.opr_opts.quiet.v e

let kind_of_ext fname =
  match Filename.extension fname with
  | ".ml" | ".mlt" | ".eliom" -> Some Syntax.Use_file
  | ".mli" | ".eliomi" -> Some Syntax.Signature
  | ".mld" -> Some Syntax.Documentation
  | _ -> None

let run config_filenames input_name =
  match input_name with
  | None ->
      Error [(fun () -> Out_channel.prerr_endline "Must specify --name")]
  | Some input_name -> (
    match kind_of_ext input_name with
    | None ->
        Error
          [ (fun () ->
              Out_channel.prerr_endline
                "--name must have extension be one of .ml, .mlt, .eliom, \
                 .mli, .eliomi, .mld." ) ]
    | Some kind -> (
        let conf = build_config config_filenames in
        match conf with
        | Error e -> Error [(fun () -> Out_channel.prerr_endline e)]
        | Ok conf -> (
            let source = In_channel.input_all In_channel.stdin in
            match format ~kind ~input_name ~source conf with
            | Ok s ->
                Out_channel.flush Out_channel.stdout ;
                Out_channel.set_binary_mode Out_channel.stdout true ;
                Out_channel.output_string Out_channel.stdout s ;
                Out_channel.flush Out_channel.stdout ;
                Out_channel.set_binary_mode Out_channel.stdout false ;
                Ok ()
            | Error e -> Error [(fun () -> print_error conf e)] ) ) )

let run config_filenames input_names =
  match run config_filenames input_names with
  | Ok () -> ()
  | Error es -> List.iter es ~f:(fun e -> e ())

open Cmdliner

let info =
  let doc = "A tool to format OCaml code." in
  let man =
    [ `S Cmdliner.Manpage.s_description
    ; `P "$(tname) automatically formats OCaml code."
    ; `S Conf.docs
    ; `P
        "Unless otherwise noted, any option \
         $(b,--)$(i,option)$(b,=)$(i,VAL) detailed in this section can be \
         set in many ways, its value is determined in the following order \
         (of increasing priority): the default value is used if no other \
         value is specified. The value of a boolean option $(b,--foo) or \
         $(b,--no-foo) can be modified in an $(b,.ocamlformat) \
         configuration file with '$(b,foo = ){$(b,true),$(b,false)}', it \
         can be done for any other option with an '$(b,option = )$(i,VAL)' \
         line (*), or using the OCAMLFORMAT environment variable: \
         $(b,OCAMLFORMAT=)$(i,option)$(b,=)$(i,VAL)$(b,,)...$(b,,)$(i,option)$(b,=)$(i,VAL), \
         or as an optional parameter on the command line, or with a global \
         $(b,[@@@ocamlformat \")$(i,option)$(b,=)$(i,VAL)$(b,\"]) attribute \
         in the processed file, or with an $(b,[@@ocamlformat \
         \")$(i,option)$(b,=)$(i,VAL)$(b,\"]) attribute on expression in \
         the processed file." ]
  in
  Cmd.info "ocamlformat" ~version:Version.current ~doc ~man

let conf_location =
  let doc = "$(docv) is the location of the configuration file." in
  Arg.(
    value & opt_all string []
    & info ["c"; "config"] ~docv:".OCAMLFORMAT" ~doc ~docs:Conf.docs )

let input_name =
  let docv = "NAME" in
  let doc =
    "Name of input file for use in error reporting and starting point when \
     searching for '.ocamlformat' files. Some options can be specified in \
     configuration files named '.ocamlformat' in the same or a parent \
     directory of $(docv), see documentation of other options for details."
  in
  Arg.(
    value & opt (some string) None & info ["name"] ~doc ~docs:Conf.docs ~docv )

let lightformat_t = Term.(const run $ conf_location $ input_name)

let lighformat = Cmd.v info lightformat_t

let () = Caml.exit @@ Cmdliner.Cmd.eval lighformat
