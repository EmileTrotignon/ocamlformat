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

module Location = Migrate_ast.Location
include Conf_t

let profile_option_names = ["p"; "profile"]

open Cmdliner

let warn_raw, collect_warnings =
  let delay_warning = ref false in
  let delayed_warning_list = ref [] in
  let warn_ s =
    if !delay_warning then delayed_warning_list := s :: !delayed_warning_list
    else Format.eprintf "%s%!" s
  in
  let collect_warnings f =
    let old_flag, old_list = (!delay_warning, !delayed_warning_list) in
    delay_warning := true ;
    delayed_warning_list := [] ;
    let res = f () in
    let collected = List.rev !delayed_warning_list in
    delay_warning := old_flag ;
    delayed_warning_list := old_list ;
    (res, fun () -> List.iter ~f:warn_ collected)
  in
  (warn_, collect_warnings)

let warn ~loc fmt =
  Format.kasprintf
    (fun s ->
      warn_raw
        (Format.asprintf "%!@{<loc>%a@}:@,@{<warning>Warning@}: %s\n%!"
           Location.print_loc loc s ) )
    fmt

module Decl = Conf_decl
module Store = Decl.Store

let conventional_profile from =
  let elt content = Elt.make content from in
  { align_pattern_matching_bar= elt `Paren
  ; assignment_operator= elt `End_line
  ; break_before_in= elt `Fit_or_vertical
  ; break_cases= elt `Fit
  ; break_collection_expressions= elt `Fit_or_vertical
  ; break_colon= elt `After
  ; break_infix= elt `Wrap
  ; break_infix_before_func= elt false
  ; break_fun_decl= elt `Wrap
  ; break_fun_sig= elt `Wrap
  ; break_separators= elt `After
  ; break_sequences= elt true
  ; break_string_literals= elt `Auto
  ; break_struct= elt true
  ; cases_exp_indent= elt 4
  ; cases_matching_exp_indent= elt `Normal
  ; disambiguate_non_breaking_match= elt false
  ; doc_comments= elt `After_when_possible
  ; doc_comments_padding= elt 2
  ; doc_comments_tag_only= elt `Default
  ; dock_collection_brackets= elt true
  ; exp_grouping= elt `Parens
  ; extension_indent= elt 2
  ; field_space= elt `Loose
  ; function_indent= elt 2
  ; function_indent_nested= elt `Never
  ; if_then_else= elt `Compact
  ; indent_after_in= elt 0
  ; indicate_multiline_delimiters= elt `No
  ; indicate_nested_or_patterns= elt `Unsafe_no
  ; infix_precedence= elt `Indent
  ; leading_nested_match_parens= elt false
  ; let_and= elt `Compact
  ; let_binding_indent= elt 2
  ; let_binding_spacing= elt `Compact
  ; let_module= elt `Compact
  ; line_endings= elt `Lf
  ; margin= elt 80
  ; match_indent= elt 0
  ; match_indent_nested= elt `Never
  ; max_indent= elt None
  ; module_item_spacing= elt `Compact
  ; nested_match= elt `Wrap
  ; ocp_indent_compat= elt false
  ; parens_ite= elt false
  ; parens_tuple= elt `Always
  ; parens_tuple_patterns= elt `Multi_line_only
  ; parse_docstrings= elt false
  ; parse_toplevel_phrases= elt false
  ; sequence_blank_line= elt `Preserve_one
  ; sequence_style= elt `Terminator
  ; single_case= elt `Compact
  ; space_around_arrays= elt true
  ; space_around_lists= elt true
  ; space_around_records= elt true
  ; space_around_variants= elt true
  ; stritem_extension_indent= elt 0
  ; type_decl= elt `Compact
  ; type_decl_indent= elt 2
  ; wrap_comments= elt false
  ; wrap_fun_args= elt true }

let default_profile = conventional_profile

let ocamlformat_profile from =
  let elt content = Elt.make content from in
  { align_pattern_matching_bar= elt `Paren
  ; assignment_operator= elt `End_line
  ; break_before_in= elt `Fit_or_vertical
  ; break_cases= elt `Nested
  ; break_collection_expressions= elt `Fit_or_vertical
  ; break_colon= elt `After
  ; break_infix= elt `Wrap
  ; break_infix_before_func= elt true
  ; break_fun_decl= elt `Wrap
  ; break_fun_sig= elt `Wrap
  ; break_separators= elt `Before
  ; break_sequences= elt false
  ; break_string_literals= elt `Auto
  ; break_struct= elt true
  ; cases_exp_indent= elt 4
  ; cases_matching_exp_indent= elt `Compact
  ; disambiguate_non_breaking_match= elt false
  ; doc_comments= elt `Before_except_val
  ; doc_comments_padding= elt 2
  ; doc_comments_tag_only= elt `Default
  ; dock_collection_brackets= elt false
  ; exp_grouping= elt `Parens
  ; extension_indent= elt 2
  ; field_space= elt `Tight
  ; function_indent= elt 2
  ; function_indent_nested= elt `Never
  ; if_then_else= elt `Compact
  ; indent_after_in= elt 0
  ; indicate_multiline_delimiters= elt `Space
  ; indicate_nested_or_patterns= elt `Space
  ; infix_precedence= elt `Indent
  ; leading_nested_match_parens= elt false
  ; let_and= elt `Compact
  ; let_binding_indent= elt 2
  ; let_binding_spacing= elt `Compact
  ; let_module= elt `Compact
  ; line_endings= elt `Lf
  ; margin= elt 80
  ; match_indent= elt 0
  ; match_indent_nested= elt `Never
  ; max_indent= elt None
  ; module_item_spacing= elt `Sparse
  ; nested_match= elt `Wrap
  ; ocp_indent_compat= elt false
  ; parens_ite= elt false
  ; parens_tuple= elt `Always
  ; parens_tuple_patterns= elt `Multi_line_only
  ; parse_docstrings= elt false
  ; parse_toplevel_phrases= elt false
  ; sequence_blank_line= elt `Compact
  ; sequence_style= elt `Separator
  ; single_case= elt `Compact
  ; space_around_arrays= elt false
  ; space_around_lists= elt false
  ; space_around_records= elt false
  ; space_around_variants= elt false
  ; stritem_extension_indent= elt 0
  ; type_decl= elt `Compact
  ; type_decl_indent= elt 2
  ; wrap_comments= elt false
  ; wrap_fun_args= elt true }

let janestreet_profile from =
  let elt content = Elt.make content from in
  { align_pattern_matching_bar= elt `Keyword
  ; assignment_operator= elt `Begin_line
  ; break_before_in= elt `Fit_or_vertical
  ; break_cases= elt `Fit_or_vertical
  ; break_collection_expressions=
      elt (ocamlformat_profile from).break_collection_expressions.v
  ; break_colon= elt `Before
  ; break_infix= elt `Fit_or_vertical
  ; break_infix_before_func= elt true
  ; break_fun_decl= elt `Fit_or_vertical
  ; break_fun_sig= elt `Fit_or_vertical
  ; break_separators= elt `Before
  ; break_sequences= elt true
  ; break_string_literals= elt `Auto
  ; break_struct= elt (ocamlformat_profile from).break_struct.v
  ; cases_exp_indent= elt 2
  ; cases_matching_exp_indent= elt `Normal
  ; disambiguate_non_breaking_match= elt false
  ; doc_comments= elt `Before
  ; doc_comments_padding= elt 1
  ; doc_comments_tag_only= elt `Fit
  ; dock_collection_brackets= elt false
  ; exp_grouping= elt `Parens
  ; extension_indent= elt 2
  ; field_space= elt `Loose
  ; function_indent= elt 2
  ; function_indent_nested= elt `Never
  ; if_then_else= elt `Keyword_first
  ; indent_after_in= elt 0
  ; indicate_multiline_delimiters= elt `No
  ; indicate_nested_or_patterns= elt `Unsafe_no
  ; infix_precedence= elt `Parens
  ; leading_nested_match_parens= elt true
  ; let_and= elt `Sparse
  ; let_binding_indent= elt 2
  ; let_binding_spacing= elt `Double_semicolon
  ; let_module= elt `Sparse
  ; line_endings= elt `Lf
  ; margin= elt 90
  ; match_indent= elt 0
  ; match_indent_nested= elt `Never
  ; max_indent= elt @@ Some 2
  ; module_item_spacing= elt `Compact
  ; nested_match= elt `Wrap
  ; ocp_indent_compat= elt true
  ; parens_ite= elt true
  ; parens_tuple= elt `Multi_line_only
  ; parens_tuple_patterns= elt `Multi_line_only
  ; parse_docstrings= elt false
  ; parse_toplevel_phrases= elt false
  ; sequence_blank_line= elt `Compact
  ; sequence_style= elt `Terminator
  ; single_case= elt `Sparse
  ; space_around_arrays= elt true
  ; space_around_lists= elt true
  ; space_around_records= elt true
  ; space_around_variants= elt true
  ; stritem_extension_indent= elt 0
  ; type_decl= elt `Sparse
  ; type_decl_indent= elt 2
  ; wrap_comments= elt false
  ; wrap_fun_args= elt false }

let add_option options opt = (Store.add options opt, opt)

let add_removed options opt = Store.add options opt

module V = struct
  let v0_12 = Version.make ~major:0 ~minor:12 ~patch:None

  let v0_14 = Version.make ~major:0 ~minor:14 ~patch:None

  let v0_16 = Version.make ~major:0 ~minor:16 ~patch:None

  let v0_17 = Version.make ~major:0 ~minor:17 ~patch:None

  let v0_22 = Version.make ~major:0 ~minor:22 ~patch:None
end

(** Options affecting formatting *)
module Formatting = struct
  let options = Store.empty

  let kind = Decl.Formatting

  let update ~f c = {c with fmt_opts= f c.fmt_opts}

  let options (*, align_cases *) =
    add_removed options
    @@
    let names = ["align-cases"] in
    Decl.removed_option ~names ~since:V.v0_22 ~msg:""

  let options (*, align_constructors_decl *) =
    add_removed options
    @@
    let names = ["align-constructors-decl"] in
    Decl.removed_option ~names ~since:V.v0_22 ~msg:""

  let options (*, align_variants_decl *) =
    add_removed options
    @@
    let names = ["align-variants-decl"] in
    Decl.removed_option ~names ~since:V.v0_22 ~msg:""

  let options, assignment_operator =
    add_option options
    @@
    let doc = "Position of the assignment operator." in
    let names = ["assignment-operator"] in
    let all =
      [ Decl.Value.make ~name:"end-line" `End_line
          "$(b,end-line) positions assignment operators (`:=` and `<-`) at \
           the end of the line and breaks after it if the whole assignment \
           expression does not fit on a single line."
      ; Decl.Value.make ~name:"begin-line" `Begin_line
          "$(b,begin-line) positions assignment operators (`:=` and `<-`) \
           at the beginning of the line and breaks before it if the whole \
           assignment expression does not fit on a single line." ]
    in
    Decl.choice ~names ~all ~doc ~kind
      (fun conf elt ->
        update conf ~f:(fun f -> {f with assignment_operator= elt}) )
      (fun conf -> conf.fmt_opts.assignment_operator)

  let options, break_before_in =
    add_option options
    @@
    let doc =
      "Whether the line should break before the $(i,in) keyword of a \
       $(i,let) binding."
    in
    let names = ["break-before-in"] in
    let all =
      [ Decl.Value.make ~name:"fit-or-vertical" `Fit_or_vertical
          "$(b,fit-or-vertical) will always break the line before the \
           $(i,in) keyword if the whole $(i,let) binding does not fit on a \
           single line."
      ; Decl.Value.make ~name:"auto" `Auto
          "$(b,auto) will only break the line if the $(i,in) keyword does \
           not fit on the previous line." ]
    in
    Decl.choice ~names ~all ~doc ~kind
      (fun conf elt ->
        update conf ~f:(fun f -> {f with break_before_in= elt}) )
      (fun conf -> conf.fmt_opts.break_before_in)

  let options, break_cases =
    add_option options
    @@
    let doc = "Break pattern match cases." in
    let names = ["break-cases"] in
    let all =
      [ Decl.Value.make ~name:"fit" `Fit
          "Specifying $(b,fit) lets pattern matches break at the margin \
           naturally."
      ; Decl.Value.make ~name:"nested" `Nested
          "$(b,nested) forces a break after nested or-patterns to highlight \
           the case body. Note that with $(b,nested), the \
           $(b,indicate-nested-or-patterns) option is not needed, and so \
           ignored."
      ; Decl.Value.make ~name:"toplevel" `Toplevel
          "$(b,toplevel) forces top-level cases (i.e. not nested \
           or-patterns) to break across lines, otherwise break naturally at \
           the margin."
      ; Decl.Value.make ~name:"fit-or-vertical" `Fit_or_vertical
          "$(b,fit-or-vertical) tries to fit all or-patterns on the same \
           line, otherwise breaks."
      ; Decl.Value.make ~name:"vertical" `Vertical
          "$(b,vertical) vertically breaks branches."
      ; Decl.Value.make ~name:"all" `All
          "$(b,all) forces all pattern matches to break across lines." ]
    in
    Decl.choice ~names ~all ~doc ~kind
      (fun conf elt -> update conf ~f:(fun f -> {f with break_cases= elt}))
      (fun conf -> conf.fmt_opts.break_cases)

  let options, break_collection_expressions =
    add_option options
    @@
    let doc =
      "Break collection expressions (lists and arrays) elements by elements."
    in
    let names = ["break-collection-expressions"] in
    let all =
      [ Decl.Value.make ~name:"fit-or-vertical" `Fit_or_vertical
          "$(b,fit-or-vertical) vertically breaks expressions if they do \
           not fit on a single line."
      ; Decl.Value.make ~name:"wrap" `Wrap
          "$(b,wrap) will group simple expressions and try to format them \
           in a single line." ]
    in
    Decl.choice ~names ~all ~doc ~kind
      (fun conf elt ->
        update conf ~f:(fun f -> {f with break_collection_expressions= elt})
        )
      (fun conf -> conf.fmt_opts.break_collection_expressions)

  let options, break_colon =
    add_option options
    @@
    let doc =
      "Break before or after colon in value binding declarations and type \
       constraints."
    in
    let names = ["break-colon"] in
    let all =
      [ Decl.Value.make ~name:"after" `After
          "$(b,after) breaks after the colon."
      ; Decl.Value.make ~name:"before" `Before
          "$(b,before) breaks before the colon." ]
    in
    Decl.choice ~names ~all ~doc ~kind
      (fun conf elt -> update conf ~f:(fun f -> {f with break_colon= elt}))
      (fun conf -> conf.fmt_opts.break_colon)

  let options, break_fun_decl =
    add_option options
    @@
    let doc = "Style for function declarations and types." in
    let names = ["break-fun-decl"] in
    let all =
      [ Decl.Value.make ~name:"wrap" `Wrap
          "$(b,wrap) breaks only if necessary."
      ; Decl.Value.make ~name:"fit-or-vertical" `Fit_or_vertical
          "$(b,fit-or-vertical) vertically breaks arguments if they do not \
           fit on a single line."
      ; Decl.Value.make ~name:"smart" `Smart
          "$(b,smart) is like $(b,fit-or-vertical) but try to fit arguments \
           on their line if they fit." ]
    in
    Decl.choice ~names ~all ~doc ~kind
      (fun conf elt -> update conf ~f:(fun f -> {f with break_fun_decl= elt}))
      (fun conf -> conf.fmt_opts.break_fun_decl)

  let options, break_fun_sig =
    add_option options
    @@
    let doc = "Style for function signatures." in
    let names = ["break-fun-sig"] in
    let all =
      [ Decl.Value.make ~name:"wrap" `Wrap
          "$(b,wrap) breaks only if necessary."
      ; Decl.Value.make ~name:"fit-or-vertical" `Fit_or_vertical
          "$(b,fit-or-vertical) vertically breaks arguments if they do not \
           fit on a single line."
      ; Decl.Value.make ~name:"smart" `Smart
          "$(b,smart) is like $(b,fit-or-vertical) but try to fit arguments \
           on their line if they fit." ]
    in
    Decl.choice ~names ~all ~doc ~kind
      (fun conf elt -> update conf ~f:(fun f -> {f with break_fun_sig= elt}))
      (fun conf -> conf.fmt_opts.break_fun_sig)

  let options, break_infix =
    add_option options
    @@
    let doc = "Break sequence of infix operators." in
    let names = ["break-infix"] in
    let all =
      [ Decl.Value.make ~name:"wrap" `Wrap
          "$(b,wrap) will group simple expressions and try to format them \
           in a single line."
      ; Decl.Value.make ~name:"fit-or-vertical" `Fit_or_vertical
          "$(b,fit-or-vertical) vertically breaks expressions if they do \
           not fit on a single line."
      ; Decl.Value.make ~name:"wrap-or-vertical" `Wrap_or_vertical
          "$(b,wrap-or-vertical) behaves like $(b,wrap) for high precedence \
           operators and behaves like $(b,fit-or-vertical) for low \
           precedence operators." ]
    in
    Decl.choice ~names ~all ~doc ~kind
      (fun conf elt -> update conf ~f:(fun f -> {f with break_infix= elt}))
      (fun conf -> conf.fmt_opts.break_infix)

  let options, break_infix_before_func =
    add_option options
    @@
    let doc =
      "Break infix operators whose right arguments are anonymous functions \
       specially: do not break after the operator so that the first line of \
       the function appears docked at the end of line after the operator."
    in
    let names = ["break-infix-before-func"] in
    Decl.flag ~default:false ~names ~doc ~kind
      (fun conf elt ->
        update conf ~f:(fun f -> {f with break_infix_before_func= elt}) )
      (fun conf -> conf.fmt_opts.break_infix_before_func)

  let options, break_separators =
    add_option options
    @@
    let doc =
      "Break before or after separators such as `;` in list or record \
       expressions."
    in
    let names = ["break-separators"] in
    let all =
      [ Decl.Value.make ~name:"after" `After
          "$(b,after) breaks the expressions after the separator."
      ; Decl.Value.make ~name:"before" `Before
          "$(b,before) breaks the expressions before the separator." ]
    in
    Decl.choice ~names ~all ~doc ~kind
      ~removed_values:
        [ Decl.Value_removed.make ~name:"after-and-docked" ~since:V.v0_12
            ~msg:
              "One can get a similar behaviour by setting \
               `break-separators=after`, `space-around-lists=false`, and \
               `dock-collection-brackets=false`." ]
      (fun conf elt ->
        update conf ~f:(fun f -> {f with break_separators= elt}) )
      (fun conf -> conf.fmt_opts.break_separators)

  let options, break_sequences =
    add_option options
    @@
    let doc =
      "Force sequence expressions to break irrespective of margin."
    in
    let names = ["break-sequences"] in
    Decl.flag ~default:true ~names ~doc ~kind
      (fun conf elt ->
        update conf ~f:(fun f -> {f with break_sequences= elt}) )
      (fun conf -> conf.fmt_opts.break_sequences)

  let options, break_string_literals =
    add_option options
    @@
    let doc = "Break string literals." in
    let names = ["break-string-literals"] in
    let all =
      [ Decl.Value.make ~name:"auto" `Auto
          "$(b,auto) mode breaks lines at newlines and wraps string \
           literals at the margin."
      ; Decl.Value.make ~name:"never" `Never
          "$(b,never) mode formats string literals as they are parsed, in \
           particular, with escape sequences expanded." ]
    in
    Decl.choice ~names ~all ~doc ~kind
      ~removed_values:
        (Decl.Value_removed.make_list
           ~names:["newlines"; "newlines-and-wrap"; "wrap"]
           ~since:V.v0_12
           ~msg:
             "It has been replaced by the new default `auto` value, which \
              breaks lines at newlines and wraps string literals at the \
              margin." )
      (fun conf elt ->
        update conf ~f:(fun f -> {f with break_string_literals= elt}) )
      (fun conf -> conf.fmt_opts.break_string_literals)

  let options, break_struct =
    add_option options
    @@
    let doc = "Break struct-end module items." in
    let names = ["break-struct"] in
    let all =
      [ Decl.Value.make ~name:"force" `Force
          "$(b,force) will break struct-end phrases unconditionally."
      ; Decl.Value.make ~name:"natural" `Natural
          "$(b,natural) will break struct-end phrases naturally at the \
           margin." ]
    in
    Decl.choice ~names ~all ~doc ~kind
      (fun conf elt ->
        update conf ~f:(fun f ->
            {f with break_struct= Elt.make Poly.(elt.v = `Force) elt.from} )
        )
      (fun conf ->
        let elt = conf.fmt_opts.break_struct in
        if elt.v then Elt.make `Force elt.from
        else Elt.make `Natural elt.from )

  let options, cases_exp_indent =
    add_option options
    @@
    let docv = "COLS" in
    let doc =
      "Indentation of cases expressions ($(docv) columns). See also the \
       $(b,cases-matching-exp-indent) and $(b,nested-match) options."
    in
    let names = ["cases-exp-indent"] in
    Decl.int ~names ~default:4 ~doc ~docv ~kind ~allow_inline:false
      (fun conf elt ->
        update conf ~f:(fun f -> {f with cases_exp_indent= elt}) )
      (fun conf -> conf.fmt_opts.cases_exp_indent)

  let options, cases_matching_exp_indent =
    add_option options
    @@
    let doc =
      "Indentation of cases right-hand sides which are `match` or `try` \
       expressions."
    in
    let names = ["cases-matching-exp-indent"] in
    let all =
      [ Decl.Value.make ~name:"normal" `Normal
          "$(b,normal) indents as it would any other expression."
      ; Decl.Value.make ~name:"compact" `Compact
          "$(b,compact) forces an indentation of 2, unless \
           $(b,nested-match) is set to $(b,align) and we're on the last \
           case." ]
    in
    Decl.choice ~names ~all ~doc ~kind
      (fun conf elt ->
        update conf ~f:(fun f -> {f with cases_matching_exp_indent= elt}) )
      (fun conf -> conf.fmt_opts.cases_matching_exp_indent)

  let options, disambiguate_non_breaking_match =
    add_option options
    @@
    let doc =
      "Add parentheses around matching constructs that fit on a single line."
    in
    Decl.flag
      ~names:["disambiguate-non-breaking-match"]
      ~default:false ~doc ~kind
      (fun conf elt ->
        update conf ~f:(fun f ->
            {f with disambiguate_non_breaking_match= elt} ) )
      (fun conf -> conf.fmt_opts.disambiguate_non_breaking_match)

  let options, doc_comments =
    add_option options
    @@
    let doc = "Doc comments position." in
    let names = ["doc-comments"] in
    let all =
      [ Decl.Value.make ~name:"after-when-possible" `After_when_possible
          "$(b,after-when-possible) puts doc comments after the \
           corresponding code. This option has no effect on variant \
           declarations because that would change their meaning and on \
           structures, signatures and objects for readability."
      ; Decl.Value.make ~name:"before-except-val" `Before_except_val
          "$(b,before-except-val) puts doc comments before the \
           corresponding code, but puts doc comments of $(b,val) and \
           $(b,external) declarations after the corresponding declarations."
      ; Decl.Value.make ~name:"before" `Before
          "$(b,before) puts comments before the corresponding code." ]
    in
    Decl.choice ~names ~all ~doc ~kind
      ~removed_values:
        [ Decl.Value_removed.make ~name:"after" ~since:V.v0_14
            ~msg:
              "This value has been renamed `after-when-possible` to take \
               into account the technical limitations of ocamlformat, the \
               behavior is unchanged." ]
      (fun conf elt -> update conf ~f:(fun f -> {f with doc_comments= elt}))
      (fun conf -> conf.fmt_opts.doc_comments)

  let options, doc_comments_padding =
    add_option options
    @@
    let docv = "PADDING" in
    let doc =
      "Add $(docv) spaces before doc comments in type declarations."
    in
    let names = ["doc-comments-padding"] in
    Decl.int ~names ~default:2 ~doc ~docv ~kind
      (fun conf elt ->
        update conf ~f:(fun f -> {f with doc_comments_padding= elt}) )
      (fun conf -> conf.fmt_opts.doc_comments_padding)

  let options, doc_comments_tag_only =
    add_option options
    @@
    let doc = "Position of doc comments with only tags." in
    let names = ["doc-comments-tag-only"] in
    let all =
      [ Decl.Value.make ~name:"default" `Default
          "$(b,default) means no special treatment."
      ; Decl.Value.make ~name:"fit" `Fit
          "$(b,fit) puts doc comments on the same line." ]
    in
    Decl.choice ~names ~all ~doc ~kind
      (fun conf elt ->
        update conf ~f:(fun f -> {f with doc_comments_tag_only= elt}) )
      (fun conf -> conf.fmt_opts.doc_comments_tag_only)

  let options (*, doc_comments_val *) =
    add_removed options
    @@
    let names = ["doc-comments-val"] in
    let msg =
      "If you are using `doc-comments-val=before` in combination with \
       `doc-comments=before` then only `doc-comments=before` is now \
       required to achive the same behavior. If you are using \
       `doc-comments-val=before` in combination with `doc-comments=after` \
       this behavior is not available anymore. If you are using \
       `doc-comments-val=after` in combination with `doc-comments=before` \
       please now use `doc-comments=before-except-val`. If you are using \
       `doc-comments-val=after` in combination with `doc-comments=after` \
       then only `doc-comments=after-when-possible` is now required to \
       achieve the same behavior. If you are using `doc-comments-val=unset` \
       the same behavior can now be achieved by setting `doc-comments` \
       only."
    in
    Decl.removed_option ~names ~since:V.v0_16 ~msg

  let options, dock_collection_brackets =
    add_option options
    @@
    let doc =
      "Dock the brackets of lists, arrays and records, so that when the \
       collection does not fit on a single line the brackets are opened on \
       the preceding line and closed on the following line."
    in
    let names = ["dock-collection-brackets"] in
    Decl.flag ~default:true ~names ~doc ~kind
      (fun conf elt ->
        update conf ~f:(fun f -> {f with dock_collection_brackets= elt}) )
      (fun conf -> conf.fmt_opts.dock_collection_brackets)

  let concrete_syntax_preserved_msg =
    "Concrete syntax will now always be preserved."

  let options (*, escape_chars *) =
    add_removed options
    @@
    let names = ["escape-chars"] in
    let msg = concrete_syntax_preserved_msg in
    Decl.removed_option ~names ~since:V.v0_16 ~msg

  let options (*, escape_strings *) =
    add_removed options
    @@
    let names = ["escape-strings"] in
    let msg = concrete_syntax_preserved_msg in
    Decl.removed_option ~names ~since:V.v0_16 ~msg

  let options, exp_grouping =
    add_option options
    @@
    let doc = "Style of expression grouping." in
    let names = ["exp-grouping"] in
    let all =
      [ Decl.Value.make ~name:"parens" `Parens
          "$(b,parens) groups expressions using parentheses."
      ; Decl.Value.make ~name:"preserve" `Preserve
          "$(b,preserve) preserves the original grouping syntax \
           (parentheses or $(i,begin)/$(i,end))." ]
    in
    Decl.choice ~names ~all ~doc ~kind ~allow_inline:false
      (fun conf elt -> update conf ~f:(fun f -> {f with exp_grouping= elt}))
      (fun conf -> conf.fmt_opts.exp_grouping)

  let options, extension_indent =
    add_option options
    @@
    let docv = "COLS" in
    let doc =
      "Indentation of items inside extension nodes ($(docv) columns)."
    in
    let names = ["extension-indent"] in
    Decl.int ~names ~default:2 ~doc ~docv ~kind
      (fun conf elt ->
        update conf ~f:(fun f -> {f with extension_indent= elt}) )
      (fun conf -> conf.fmt_opts.extension_indent)

  let options (*, extension_sugar *) =
    add_removed options
    @@
    let names = ["extension-sugar"] in
    let msg = concrete_syntax_preserved_msg in
    Decl.removed_option ~names ~since:V.v0_17 ~msg

  let options, field_space =
    add_option options
    @@
    let doc =
      "Whether or not to use a space between a field name and the \
       punctuation symbol (`:` or `=`) preceding the rhs. This option \
       affects records and objects."
    in
    let names = ["field-space"] in
    let all =
      [ Decl.Value.make ~name:"loose" `Loose "$(b,loose) uses a space."
      ; Decl.Value.make ~name:"tight" `Tight
          "$(b,tight) does not use a space."
      ; Decl.Value.make ~name:"tight-decl" `Tight_decl
          "$(b,tight-decl) is $(b,tight) for declarations and $(b,loose) \
           for instantiations." ]
    in
    Decl.choice ~names ~all ~doc ~kind
      (fun conf elt -> update conf ~f:(fun f -> {f with field_space= elt}))
      (fun conf -> conf.fmt_opts.field_space)

  let options, function_indent =
    add_option options
    @@
    let docv = "COLS" in
    let doc = "Indentation of function cases ($(docv) columns)." in
    let names = ["function-indent"] in
    Decl.int ~names ~default:2 ~doc ~docv ~kind
      (fun conf elt ->
        update conf ~f:(fun f -> {f with function_indent= elt}) )
      (fun conf -> conf.fmt_opts.function_indent)

  let options, function_indent_nested =
    add_option options
    @@
    let doc =
      "Whether the $(b,function-indent) parameter should be applied even \
       when in a sub-block."
    in
    let names = ["function-indent-nested"] in
    let all =
      [ Decl.Value.make ~name:"never" `Never
          "$(b,never) only applies $(b,function-indent) if the function \
           block starts a line."
      ; Decl.Value.make ~name:"always" `Always
          "$(b,always) always apply $(b,function-indent)."
      ; Decl.Value.make ~name:"auto" `Auto
          "$(b,auto) applies $(b,function-indent) when seen fit." ]
    in
    Decl.choice ~names ~all ~doc ~kind
      (fun conf elt ->
        update conf ~f:(fun f -> {f with function_indent_nested= elt}) )
      (fun conf -> conf.fmt_opts.function_indent_nested)

  let options, if_then_else =
    add_option options
    @@
    let doc = "If-then-else formatting." in
    let names = ["if-then-else"] in
    let all =
      [ Decl.Value.make ~name:"compact" `Compact
          "$(b,compact) tries to format an if-then-else expression on a \
           single line."
      ; Decl.Value.make ~name:"fit-or-vertical" `Fit_or_vertical
          "$(b,fit-or-vertical) vertically breaks branches if they do not \
           fit on a single line."
      ; Decl.Value.make ~name:"vertical" `Vertical
          "$(b,vertical) always vertically breaks branches."
      ; Decl.Value.make ~name:"keyword-first" `Keyword_first
          "$(b,keyword-first) formats if-then-else expressions such that \
           the if-then-else keywords are the first on the line."
      ; Decl.Value.make ~name:"k-r" `K_R
          "$(b,k-r) formats if-then-else expressions with parentheses that \
           match the K&R style." ]
    in
    Decl.choice ~names ~all ~doc ~kind
      (fun conf elt -> update conf ~f:(fun f -> {f with if_then_else= elt}))
      (fun conf -> conf.fmt_opts.if_then_else)

  let options, indent_after_in =
    add_option options
    @@
    let docv = "COLS" in
    let doc =
      "Indentation ($(docv) columns) after `let ... in`, unless followed by \
       another `let`."
    in
    let names = ["indent-after-in"] in
    Decl.int ~names ~default:0 ~doc ~docv ~kind ~allow_inline:false
      (fun conf elt ->
        update conf ~f:(fun f -> {f with indent_after_in= elt}) )
      (fun conf -> conf.fmt_opts.indent_after_in)

  let options, indicate_multiline_delimiters =
    add_option options
    @@
    let doc =
      "How to indicate that two matching delimiters live on different lines."
    in
    let names = ["indicate-multiline-delimiters"] in
    let all =
      [ Decl.Value.make ~name:"no" `No
          "$(b, no) doesn't do anything special to indicate the closing \
           delimiter."
      ; Decl.Value.make ~name:"space" `Space
          "$(b,space) prints a space inside the delimiter to indicate the \
           matching one is on a different line."
      ; Decl.Value.make ~name:"closing-on-separate-line"
          `Closing_on_separate_line
          "$(b, closing-on-separate-line) makes sure that the closing \
           delimiter is on its own line." ]
    in
    Decl.choice ~names ~all ~doc ~kind
      (fun conf elt ->
        update conf ~f:(fun f ->
            {f with indicate_multiline_delimiters= elt} ) )
      (fun conf -> conf.fmt_opts.indicate_multiline_delimiters)

  let options, indicate_nested_or_patterns =
    add_option options
    @@
    let doc =
      "Control whether or not to indicate nested or-pattern using \
       indentation."
    in
    let names = ["indicate-nested-or-patterns"] in
    let all =
      [ Decl.Value.make ~name:"unsafe-no" `Unsafe_no
          "$(b,unsafe-no) does not indicate nested or-patterns. Warning: \
           this can produce confusing code where a short body of a match \
           case is visually hidden by surrounding long patterns, leading to \
           misassociation between patterns and body expressions."
      ; Decl.Value.make ~name:"space" `Space
          "$(b,space) starts lines of nested or-patterns with \" |\" rather \
           than \"| \"." ]
    in
    Decl.choice ~names ~all ~doc ~kind
      (fun conf elt ->
        update conf ~f:(fun f -> {f with indicate_nested_or_patterns= elt})
        )
      (fun conf -> conf.fmt_opts.indicate_nested_or_patterns)

  let options, infix_precedence =
    add_option options
    @@
    let doc =
      "Use indentation or also discretionary parentheses to explicitly \
       disambiguate precedences of infix operators."
    in
    let names = ["infix-precedence"] in
    let all =
      [ Decl.Value.make ~name:"indent" `Indent
          "$(b,indent) uses indentation to explicitly disambiguate \
           precedences of infix operators."
      ; Decl.Value.make ~name:"parens" `Parens
          "$(b,parens) uses parentheses to explicitly disambiguate \
           precedences of infix operators." ]
    in
    Decl.choice ~names ~all ~doc ~kind
      (fun conf elt ->
        update conf ~f:(fun f -> {f with infix_precedence= elt}) )
      (fun conf -> conf.fmt_opts.infix_precedence)

  let options, leading_nested_match_parens =
    add_option options
    @@
    let doc = "Nested match parens formatting." in
    let names = ["leading-nested-match-parens"] in
    Decl.flag ~default:false ~names ~doc ~kind ~allow_inline:false
      (fun conf elt ->
        update conf ~f:(fun f -> {f with leading_nested_match_parens= elt})
        )
      (fun conf -> conf.fmt_opts.leading_nested_match_parens)

  let options, let_and =
    add_option options
    @@
    let doc = "Style of let_and." in
    let names = ["let-and"] in
    let all =
      [ Decl.Value.make ~name:"compact" `Compact
          "$(b,compact) will try to format `let p = e and p = e` in a \
           single line."
      ; Decl.Value.make ~name:"sparse" `Sparse
          "$(b,sparse) will always break between them." ]
    in
    Decl.choice ~names ~all ~doc ~kind
      (fun conf elt -> update conf ~f:(fun f -> {f with let_and= elt}))
      (fun conf -> conf.fmt_opts.let_and)

  let options, let_binding_indent =
    add_option options
    @@
    let docv = "COLS" in
    let doc =
      "Indentation of let binding expressions ($(docv) columns) if they do \
       not fit on a single line."
    in
    let names = ["let-binding-indent"] in
    Decl.int ~names ~default:2 ~doc ~docv ~kind ~allow_inline:false
      (fun conf elt ->
        update conf ~f:(fun f -> {f with let_binding_indent= elt}) )
      (fun conf -> conf.fmt_opts.let_binding_indent)

  let options, let_binding_spacing =
    add_option options
    @@
    let doc = "Spacing between let binding." in
    let names = ["let-binding-spacing"] in
    let all =
      [ Decl.Value.make ~name:"compact" `Compact
          "$(b,compact) spacing separates adjacent let bindings in a module \
           according to module-item-spacing."
      ; Decl.Value.make ~name:"sparse" `Sparse
          "$(b,sparse) places two open lines between a multi-line \
           module-level let binding and the next."
      ; Decl.Value.make ~name:"double-semicolon" `Double_semicolon
          "$(b,double-semicolon) places double semicolons and an open line \
           between a multi-line module-level let binding and the next." ]
    in
    Decl.choice ~names ~all ~doc ~kind
      (fun conf elt ->
        update conf ~f:(fun f -> {f with let_binding_spacing= elt}) )
      (fun conf -> conf.fmt_opts.let_binding_spacing)

  let options, let_module =
    add_option options
    @@
    let doc = "Module binding formatting." in
    let all =
      [ Decl.Value.make ~name:"compact" `Compact
          "$(b,compact) does not break a line after the $(i,let module ... \
           =) and before the $(i,in) if the module declaration does not fit \
           on a single line."
      ; Decl.Value.make ~name:"sparse" `Sparse
          "$(b,sparse) breaks a line after $(i,let module ... =) and before \
           the $(i,in) if the module declaration does not fit on a single \
           line." ]
    in
    Decl.choice ~names:["let-module"] ~all ~doc ~kind
      (fun conf elt -> update conf ~f:(fun f -> {f with let_module= elt}))
      (fun conf -> conf.fmt_opts.let_module)

  let options (*, let_open *) =
    add_removed options
    @@
    let names = ["let-open"] in
    let msg = concrete_syntax_preserved_msg in
    Decl.removed_option ~names ~since:V.v0_17 ~msg

  let options, line_endings =
    add_option options
    @@
    let doc = "Line endings used." in
    let all =
      [ Decl.Value.make ~name:"lf" `Lf "$(b,lf) uses Unix line endings."
      ; Decl.Value.make ~name:"crlf" `Crlf
          "$(b,crlf) uses Windows line endings." ]
    in
    Decl.choice ~names:["line-endings"] ~all ~doc ~allow_inline:false ~kind
      (fun conf elt -> update conf ~f:(fun f -> {f with line_endings= elt}))
      (fun conf -> conf.fmt_opts.line_endings)

  let options, margin =
    add_option options
    @@
    let docv = "COLS" in
    let doc = "Format code to fit within $(docv) columns." in
    Decl.int ~names:["m"; "margin"] ~default:80 ~doc ~docv ~kind
      ~allow_inline:false
      (fun conf elt ->
        (* printf "updating margin : %i\n" elt.v ;
        Out_channel.flush stdout ; *)
        update conf ~f:(fun f -> {f with margin= elt}) )
      (fun conf -> conf.fmt_opts.margin)

  let options, match_indent =
    add_option options
    @@
    let docv = "COLS" in
    let doc = "Indentation of match/try cases ($(docv) columns)." in
    let names = ["match-indent"] in
    Decl.int ~names ~default:0 ~doc ~docv ~kind
      (fun conf elt -> update conf ~f:(fun f -> {f with match_indent= elt}))
      (fun conf -> conf.fmt_opts.match_indent)

  let options, match_indent_nested =
    add_option options
    @@
    let doc =
      "Whether the $(b,match-indent) parameter should be applied even when \
       in a sub-block."
    in
    let names = ["match-indent-nested"] in
    let all =
      [ Decl.Value.make ~name:"never" `Never
          "$(b,never) only applies $(b,match-indent) if the match block \
           starts a line."
      ; Decl.Value.make ~name:"always" `Always
          "$(b,always) always apply $(b,match-indent)."
      ; Decl.Value.make ~name:"auto" `Auto
          "$(b,auto) applies $(b,match-indent) when seen fit." ]
    in
    Decl.choice ~names ~all ~doc ~kind
      (fun conf elt ->
        update conf ~f:(fun f -> {f with match_indent_nested= elt}) )
      (fun conf -> conf.fmt_opts.match_indent_nested)

  let default_max_indent =
    (* Creating a fresh formatter in case the value of max-indent has been
       changed for stdout. *)
    let fs = Format.formatter_of_buffer (Buffer.create 0) in
    Int.to_string (Format.pp_get_max_indent fs ())

  let options, max_indent =
    add_option options
    @@
    let docv = "COLS" in
    let doc =
      "Maximum offset ($(docv) columns) added to a new line in addition to \
       the offset of the previous line."
    in
    Decl.any
      Arg.(some ~none:default_max_indent int)
      ~names:["max-indent"] ~doc ~docv ~kind ~default:None
      ~allow_inline:false ~values:Int
      (fun conf elt -> update conf ~f:(fun f -> {f with max_indent= elt}))
      (fun conf -> conf.fmt_opts.max_indent)

  let options, module_item_spacing =
    add_option options
    @@
    let doc = "Spacing between items of structures and signatures." in
    let names = ["module-item-spacing"] in
    let all =
      [ Decl.Value.make ~name:"compact" `Compact
          "$(b,compact) will not leave open lines between one-liners of \
           similar sorts."
      ; Decl.Value.make ~name:"sparse" `Sparse
          "$(b,sparse) will always break a line between two items."
      ; Decl.Value.make ~name:"preserve" `Preserve
          "$(b,preserve) will not leave open lines between one-liners of \
           similar sorts unless there is an open line in the input." ]
    in
    Decl.choice ~names ~all ~doc ~kind
      (fun conf elt ->
        update conf ~f:(fun f -> {f with module_item_spacing= elt}) )
      (fun conf -> conf.fmt_opts.module_item_spacing)

  let options, nested_match =
    add_option options
    @@
    let doc =
      "Style of a pattern-matching nested in the last case of another \
       pattern-matching."
    in
    let names = ["nested-match"] in
    let all =
      [ Decl.Value.make ~name:"wrap" `Wrap
          "$(b,wrap) wraps the nested pattern-matching with parentheses and \
           adds indentation."
      ; Decl.Value.make ~name:"align" `Align
          "$(b,align) vertically aligns the nested pattern-matching under \
           the encompassing pattern-matching." ]
    in
    Decl.choice ~names ~all ~doc ~kind
      (fun conf elt -> update conf ~f:(fun f -> {f with nested_match= elt}))
      (fun conf -> conf.fmt_opts.nested_match)

  let options, ocp_indent_compat =
    add_option options
    @@
    let doc =
      "Attempt to generate output which does not change (much) when \
       post-processing with ocp-indent."
    in
    let names = ["ocp-indent-compat"] in
    Decl.flag ~default:false ~names ~doc ~kind
      (fun conf elt ->
        update conf ~f:(fun f -> {f with ocp_indent_compat= elt}) )
      (fun conf -> conf.fmt_opts.ocp_indent_compat)

  let options, parens_ite =
    add_option options
    @@
    let doc =
      "Uses parentheses around if-then-else branches that spread across \
       multiple lines."
    in
    let names = ["parens-ite"] in
    Decl.flag ~default:false ~names ~doc ~kind
      (fun conf elt -> update conf ~f:(fun f -> {f with parens_ite= elt}))
      (fun conf -> conf.fmt_opts.parens_ite)

  let options, parens_tuple =
    add_option options
    @@
    let doc = "Parens tuple expressions." in
    let names = ["parens-tuple"] in
    let all =
      [ Decl.Value.make ~name:"always" `Always
          "$(b,always) always uses parentheses around tuples."
      ; Decl.Value.make ~name:"multi-line-only" `Multi_line_only
          "$(b,multi-line-only) mode will try to skip parens for \
           single-line tuples." ]
    in
    Decl.choice ~names ~all ~doc ~kind
      (fun conf elt -> update conf ~f:(fun f -> {f with parens_tuple= elt}))
      (fun conf -> conf.fmt_opts.parens_tuple)

  let options, parens_tuple_patterns =
    add_option options
    @@
    let doc = "Parens tuple patterns." in
    let names = ["parens-tuple-patterns"] in
    let all =
      [ Decl.Value.make ~name:"multi-line-only" `Multi_line_only
          "$(b,multi-line-only) mode will try to skip parens for \
           single-line tuple patterns."
      ; Decl.Value.make ~name:"always" `Always
          "$(b,always) always uses parentheses around tuples patterns." ]
    in
    Decl.choice ~names ~all ~doc ~kind
      (fun conf elt ->
        update conf ~f:(fun f -> {f with parens_tuple_patterns= elt}) )
      (fun conf -> conf.fmt_opts.parens_tuple_patterns)

  let options, parse_docstrings =
    add_option options
    @@
    let doc = "Parse and format docstrings." in
    let names = ["parse-docstrings"] in
    Decl.flag ~default:false ~names ~doc ~kind
      (fun conf elt ->
        update conf ~f:(fun f -> {f with parse_docstrings= elt}) )
      (fun conf -> conf.fmt_opts.parse_docstrings)

  let options, parse_toplevel_phrases =
    add_option options
    @@
    let doc = "Parse and format toplevel phrases and their output." in
    let names = ["parse-toplevel-phrases"] in
    Decl.flag ~default:false ~names ~doc ~kind
      (fun conf elt ->
        update conf ~f:(fun f -> {f with parse_toplevel_phrases= elt}) )
      (fun conf -> conf.fmt_opts.parse_toplevel_phrases)

  let options, sequence_blank_line =
    add_option options
    @@
    let doc = "Blank line between expressions of a sequence." in
    let names = ["sequence-blank-line"] in
    let all =
      [ Decl.Value.make ~name:"preserve-one" `Preserve_one
          "$(b,preserve) will keep a blank line between two expressions of \
           a sequence if the input contains at least one."
      ; Decl.Value.make ~name:"compact" `Compact
          "$(b,compact) will not keep any blank line between expressions of \
           a sequence." ]
    in
    Decl.choice ~names ~all ~doc ~kind
      (fun conf elt ->
        update conf ~f:(fun f -> {f with sequence_blank_line= elt}) )
      (fun conf -> conf.fmt_opts.sequence_blank_line)

  let options, sequence_style =
    add_option options
    @@
    let doc = "Style of sequence." in
    let names = ["sequence-style"] in
    let all =
      [ Decl.Value.make ~name:"terminator" `Terminator
          "$(b,terminator) only puts spaces after semicolons."
      ; Decl.Value.make ~name:"separator" `Separator
          "$(b,separator) puts spaces before and after semicolons."
      ; Decl.Value.make ~name:"before" `Before
          "$(b,before) breaks the sequence before semicolons." ]
    in
    Decl.choice ~names ~all ~doc ~kind
      (fun conf elt -> update conf ~f:(fun f -> {f with sequence_style= elt}))
      (fun conf -> conf.fmt_opts.sequence_style)

  let options, single_case =
    add_option options
    @@
    let doc =
      "Style of pattern matching expressions with only a single case."
    in
    let names = ["single-case"] in
    let all =
      [ Decl.Value.make ~name:"compact" `Compact
          "$(b,compact) will try to format a single case on a single line."
      ; Decl.Value.make ~name:"sparse" `Sparse
          "$(b,sparse) will always break the line before a single case." ]
    in
    Decl.choice ~names ~all ~doc ~kind
      (fun conf elt -> update conf ~f:(fun f -> {f with single_case= elt}))
      (fun conf -> conf.fmt_opts.single_case)

  let options, space_around_arrays =
    add_option options
    @@
    let doc = "Add a space inside the delimiters of arrays." in
    let names = ["space-around-arrays"] in
    Decl.flag ~default:true ~names ~doc ~kind
      (fun conf elt ->
        update conf ~f:(fun f -> {f with space_around_arrays= elt}) )
      (fun conf -> conf.fmt_opts.space_around_arrays)

  let options, space_around_lists =
    add_option options
    @@
    let doc = "Add a space inside the delimiters of lists." in
    let names = ["space-around-lists"] in
    Decl.flag ~default:true ~names ~doc ~kind
      (fun conf elt ->
        update conf ~f:(fun f -> {f with space_around_lists= elt}) )
      (fun conf -> conf.fmt_opts.space_around_lists)

  let options, space_around_records =
    add_option options
    @@
    let doc = "Add a space inside the delimiters of records." in
    let names = ["space-around-records"] in
    Decl.flag ~default:true ~names ~doc ~kind
      (fun conf elt ->
        update conf ~f:(fun f -> {f with space_around_records= elt}) )
      (fun conf -> conf.fmt_opts.space_around_records)

  let options, space_around_variants =
    add_option options
    @@
    let doc = "Add a space inside the delimiters of variants." in
    let names = ["space-around-variants"] in
    Decl.flag ~default:true ~names ~doc ~kind
      (fun conf elt ->
        update conf ~f:(fun f -> {f with space_around_variants= elt}) )
      (fun conf -> conf.fmt_opts.space_around_variants)

  let options, stritem_extension_indent =
    add_option options
    @@
    let docv = "COLS" in
    let doc =
      "Indentation of structure items inside extension nodes ($(docv) \
       columns)."
    in
    let names = ["stritem-extension-indent"] in
    Decl.int ~names ~default:0 ~doc ~docv ~kind
      (fun conf elt ->
        update conf ~f:(fun f -> {f with stritem_extension_indent= elt}) )
      (fun conf -> conf.fmt_opts.stritem_extension_indent)

  let options, type_decl =
    add_option options
    @@
    let doc = "Style of type declaration." in
    let names = ["type-decl"] in
    let all =
      [ Decl.Value.make ~name:"compact" `Compact
          "$(b,compact) will try to format constructors and records \
           definition in a single line."
      ; Decl.Value.make ~name:"sparse" `Sparse
          "$(b,sparse) will always break between constructors and record \
           fields." ]
    in
    Decl.choice ~names ~all ~doc ~kind
      (fun conf elt -> update conf ~f:(fun f -> {f with type_decl= elt}))
      (fun conf -> conf.fmt_opts.type_decl)

  let options, type_decl_indent =
    add_option options
    @@
    let docv = "COLS" in
    let doc =
      "Indentation of type declarations ($(docv) columns) if they do not \
       fit on a single line."
    in
    let names = ["type-decl-indent"] in
    Decl.int ~names ~default:2 ~doc ~docv ~kind ~allow_inline:false
      (fun conf elt ->
        update conf ~f:(fun f -> {f with type_decl_indent= elt}) )
      (fun conf -> conf.fmt_opts.type_decl_indent)

  let options, wrap_comments =
    add_option options
    @@
    let doc =
      "Wrap comments and docstrings. Comments and docstrings are divided \
       into paragraphs by open lines (two or more consecutive newlines), \
       and each paragraph is wrapped at the margin. Multi-line comments \
       with vertically-aligned asterisks on the left margin are not \
       wrapped. Consecutive comments with both left and right margin \
       aligned are not wrapped either."
    in
    Decl.flag ~default:false ~names:["wrap-comments"] ~doc ~kind
      (fun conf elt -> update conf ~f:(fun f -> {f with wrap_comments= elt}))
      (fun conf -> conf.fmt_opts.wrap_comments)

  let options, wrap_fun_args =
    add_option options
    @@
    let default = true in
    let doc = "Style for function call." in
    let names = ["wrap-fun-args"] in
    Decl.flag ~default ~names ~doc ~kind
      (fun conf elt -> update conf ~f:(fun f -> {f with wrap_fun_args= elt}))
      (fun conf -> conf.fmt_opts.wrap_fun_args)
end

(* Flags that can be modified in the config file that don't affect
   formatting *)

let kind = Decl.Operational

module Operational = struct
  let update ~f c = {c with opr_opts= f c.opr_opts}

  let options = Store.empty

  let options, comment_check =
    add_option options
    @@
    let default = true in
    let doc =
      "Control whether to check comments and documentation comments. Unsafe \
       to turn off. May be set in $(b,.ocamlformat)."
    in
    Decl.flag ~default ~names:["comment-check"] ~doc ~kind
      (fun conf elt -> update conf ~f:(fun f -> {f with comment_check= elt}))
      (fun conf -> conf.opr_opts.comment_check)

  let options, debug =
    add_option options
    @@
    let doc = "Generate debugging output." in
    let default = false in
    Decl.flag ~default ~names:["g"; "debug"] ~doc ~kind
      (fun conf elt -> update conf ~f:(fun f -> {f with debug= elt}))
      (fun conf -> conf.opr_opts.debug)

  let options, disable =
    add_option options
    @@
    let doc =
      "Disable ocamlformat. This is used in attributes to locally disable \
       automatic code formatting. One can also use $(b,[@@@ocamlformat \
       \"enable\"]) instead of $(b,[@@@ocamlformat \"disable=false\"])."
    in
    Decl.flag ~names:["disable"] ~default:false ~doc ~kind ~allow_inline:true
      (fun conf elt -> update conf ~f:(fun f -> {f with disable= elt}))
      (fun conf -> conf.opr_opts.disable)

  let options, margin_check =
    add_option options
    @@
    let doc = "Emit a warning if the formatted output exceeds the margin." in
    Decl.flag ~default:false ~names:["margin-check"] ~doc ~kind
      (fun conf elt -> update conf ~f:(fun f -> {f with margin_check= elt}))
      (fun conf -> conf.opr_opts.margin_check)

  let options, max_iters =
    add_option options
    @@
    let docv = "N" in
    let doc =
      "Fail if output of formatting does not stabilize within $(docv) \
       iterations. May be set in $(b,.ocamlformat)."
    in
    Decl.int ~names:["n"; "max-iters"] ~default:10 ~doc ~docv ~kind
      (fun conf elt -> update conf ~f:(fun f -> {f with max_iters= elt}))
      (fun conf -> conf.opr_opts.max_iters)

  let options, ocaml_version =
    add_option options
    @@
    let doc = "Version of OCaml syntax of the output." in
    let default = Ocaml_version.Releases.v4_04_0 in
    Decl.ocaml_version ~names:["ocaml-version"] ~default ~doc ~kind
      (fun conf elt -> update conf ~f:(fun f -> {f with ocaml_version= elt}))
      (fun conf -> conf.opr_opts.ocaml_version)

  let options, quiet =
    add_option options
    @@
    let doc = "Quiet. May be set in $(b,.ocamlformat)." in
    Decl.flag ~default:false ~names:["q"; "quiet"] ~doc ~kind
      (fun conf elt -> update conf ~f:(fun f -> {f with quiet= elt}))
      (fun conf -> conf.opr_opts.quiet)

  let options, range =
    add_option options
    @@
    let doc =
      "Apply the formatting to a range of lines. Must be included between 1 \
       and the number of lines of the input. If a range is invalid the \
       whole input is considered. Warning: only supported in conbination \
       with `--numeric` for now."
    in
    let default = Range.make ?range:None in
    let docv = "X-Y" in
    Decl.range ~names:["range"] ~default ~doc ~docv ~kind
      (fun conf elt -> update conf ~f:(fun f -> {f with range= elt}))
      (fun conf -> conf.opr_opts.range)

  let options, disable_conf_attrs =
    add_option options
    @@
    let doc = "Disable configuration in attributes." in
    Decl.flag ~default:false ~names:["disable-conf-attrs"] ~doc ~kind
      (fun conf elt ->
        update conf ~f:(fun f -> {f with disable_conf_attrs= elt}) )
      (fun conf -> conf.opr_opts.disable_conf_attrs)

  let options, version_check =
    add_option options
    @@
    let doc =
      "Check that the version matches the one specified in .ocamlformat."
    in
    Decl.flag ~default:true ~names:["version-check"] ~doc ~kind
      (fun conf elt -> update conf ~f:(fun f -> {f with version_check= elt}))
      (fun conf -> conf.opr_opts.version_check)
end

let options = Store.merge Operational.options Formatting.options

let options (*, disable_outside_detected_project *) =
  add_removed options
  @@
  let msg =
    "OCamlFormat is disabled outside of a detected project by default, to \
     enable the opposite behavior use `enable-outside-detected-project`."
  in
  let names = ["disable-outside-detected-project"] in
  Decl.removed_option ~names ~since:V.v0_22 ~msg

let options, profile =
  add_option options
  @@
  let doc =
    "Select a preset profile which sets $(i,all) options, overriding lower \
     priority configuration."
  in
  let names = profile_option_names in
  let all =
    [ Decl.Value.make ~name:"conventional" `default
        "The $(b,conventional) profile aims to be as familiar and \
         \"conventional\" appearing as the available options allow."
    ; Decl.Value.make ~name:"default" `default
        "$(b,default) is an alias for the $(b,conventional) profile."
    ; Decl.Value.make ~name:"ocamlformat" `ocamlformat
        "The $(b,ocamlformat) profile aims to take advantage of the \
         strengths of a parsetree-based auto-formatter, and to limit the \
         consequences of the weaknesses imposed by the current \
         implementation. This is a style which optimizes for what the \
         formatter can do best, rather than to match the style of any \
         existing code. General guidelines that have directed the design \
         include: Legibility, in the sense of making it as hard as possible \
         for quick visual parsing to give the wrong interpretation, is of \
         highest priority; Whenever possible the high-level structure of \
         the code should be obvious by looking only at the left margin, in \
         particular, it should not be necessary to visually jump from left \
         to right hunting for critical keywords, tokens, etc; All else \
         equal compact code is preferred as reading without scrolling is \
         easier, so indentation or white space is avoided unless it helps \
         legibility; Attention has been given to making some syntactic \
         gotchas visually obvious."
    ; Decl.Value.make ~name:"janestreet" `janestreet
        "The $(b,janestreet) profile is used at Jane Street." ]
  in
  Decl.choice ~names ~all ~doc ~kind:Decl.Formatting
    ~removed_values:
      [ Decl.Value_removed.make ~name:"compact" ~since:V.v0_22 ~msg:""
      ; Decl.Value_removed.make ~name:"sparse" ~since:V.v0_22 ~msg:"" ]
    (fun conf elt ->
      let p = elt.v and from = elt.from in
      let name =
        match p with
        | `default -> "default"
        | `ocamlformat -> "ocamlformat"
        | `janestreet -> "janestreet"
      in
      (* printf "setting profile : %s\n" name ;
      Out_channel.flush stdout ; *)
      let from_p =
        let ufrom =
          match from with
          | `Default | `Updated (_, Some _) -> assert false
          | `Profile (_, uf) | `Updated (uf, _) -> uf
        in
        `Profile (name, ufrom)
      in
      let p =
        ( match p with
        | `default -> default_profile
        | `ocamlformat -> ocamlformat_profile
        | `janestreet -> janestreet_profile )
          from_p
      in
      {conf with profile= elt; fmt_opts= p} )
    (fun conf -> conf.profile)

let parse_line config ?(version_check = config.opr_opts.version_check.v)
    ?(disable_conf_attrs = config.opr_opts.disable_conf_attrs.v) ~from s =
  let update ~config ~from ~name ~value =
    let name = String.strip name in
    let value = String.strip value in
    match (name, from) with
    | "version", `File _ ->
        if String.equal Version.current value || not version_check then
          Ok config
        else
          Error
            (Error.Version_mismatch {read= value; installed= Version.current})
    | name, `File x ->
        Decl.update options ~config
          ~from:(`Parsed (`File x))
          ~name ~value ~inline:false
    | name, `Attribute loc ->
        if disable_conf_attrs then (
          warn ~loc "Configuration in attribute %S ignored." s ;
          Ok config )
        else
          Decl.update options ~config
            ~from:(`Parsed (`Attribute loc))
            ~name ~value ~inline:true
  in
  let s =
    match String.index s '#' with
    | Some i -> String.sub s ~pos:0 ~len:i
    | None -> s
  in
  let s = String.strip s in
  match String.split ~on:'=' s with
  | [] | [""] -> Ok config
  | [name; value] ->
      let name = String.strip name in
      let value = String.strip value in
      update ~config ~from ~name ~value
  | [s] -> (
    match String.strip s with
    | "" -> impossible "previous match"
    (* special case for disable/enable *)
    | "enable" -> update ~config ~from ~name:"disable" ~value:"false"
    | name -> update ~config ~from ~name ~value:"true" )
  | _ -> Error (Error.Malformed s)

let default =
  let elt content = Elt.make content `Default in
  let default_elt opt = elt @@ Decl.default opt in
  { fmt_opts= default_profile `Default
  ; profile= elt `default
  ; opr_opts=
      { comment_check= default_elt Operational.comment_check
      ; debug= default_elt Operational.debug
      ; disable= default_elt Operational.disable
      ; margin_check= default_elt Operational.margin_check
      ; max_iters= default_elt Operational.max_iters
      ; ocaml_version= default_elt Operational.ocaml_version
      ; quiet= default_elt Operational.quiet
      ; range= default_elt Operational.range
      ; disable_conf_attrs= default_elt Operational.disable_conf_attrs
      ; version_check= default_elt Operational.version_check } }

open Parsetree

let update ?(quiet = false) c {attr_name= {txt; loc}; attr_payload; _} =
  let result =
    match txt with
    | "ocamlformat" -> (
      match attr_payload with
      | PStr
          [ { pstr_desc=
                Pstr_eval
                  ( { pexp_desc=
                        Pexp_constant
                          {pconst_desc= Pconst_string (str, strloc, None); _}
                    ; pexp_attributes= []
                    ; _ }
                  , [] )
            ; _ } ] ->
          parse_line ~from:(`Attribute strloc) c str
          |> Result.map_error ~f:Error.to_string
      | _ -> Error "Invalid format: String expected" )
    | _ when String.is_prefix ~prefix:"ocamlformat." txt ->
        Error
          (Format.sprintf "Invalid format: Unknown suffix %S"
             (String.chop_prefix_exn ~prefix:"ocamlformat." txt) )
    | _ -> Ok c
  in
  match result with
  | Ok conf -> conf
  | Error error ->
      let w = Warnings.Attribute_payload (txt, error) in
      if (not c.opr_opts.quiet.v) && not quiet then
        Warning.print_warning loc w ;
      c

let update_value config ~name ~value =
  Decl.update options ~config ~from:`Commandline ~name ~value ~inline:false

let update_state c state =
  let disable = match state with `Enable -> false | `Disable -> true in
  let opr_opts =
    {c.opr_opts with disable= {c.opr_opts.disable with v= disable}}
  in
  {c with opr_opts}

let print_config = Decl.print_config options

module UI = struct
  let profile = Decl.to_ui profile

  let opr_opts =
    let open Operational in
    [Decl.to_ui ocaml_version; Decl.to_ui range]

  let fmt_opts =
    let open Formatting in
    [ Decl.to_ui assignment_operator
    ; Decl.to_ui break_before_in
    ; Decl.to_ui break_cases
    ; Decl.to_ui break_collection_expressions
    ; Decl.to_ui break_colon
    ; Decl.to_ui break_infix
    ; Decl.to_ui break_infix_before_func
    ; Decl.to_ui break_fun_decl
    ; Decl.to_ui break_fun_sig
    ; Decl.to_ui break_separators
    ; Decl.to_ui break_sequences
    ; Decl.to_ui break_string_literals
    ; Decl.to_ui break_struct
    ; Decl.to_ui cases_exp_indent
    ; Decl.to_ui cases_matching_exp_indent
    ; Decl.to_ui disambiguate_non_breaking_match
    ; Decl.to_ui doc_comments
    ; Decl.to_ui doc_comments_padding
    ; Decl.to_ui doc_comments_tag_only
    ; Decl.to_ui dock_collection_brackets
    ; Decl.to_ui exp_grouping
    ; Decl.to_ui extension_indent
    ; Decl.to_ui field_space
    ; Decl.to_ui function_indent
    ; Decl.to_ui function_indent_nested
    ; Decl.to_ui if_then_else
    ; Decl.to_ui indent_after_in
    ; Decl.to_ui indicate_multiline_delimiters
    ; Decl.to_ui indicate_nested_or_patterns
    ; Decl.to_ui infix_precedence
    ; Decl.to_ui leading_nested_match_parens
    ; Decl.to_ui let_and
    ; Decl.to_ui let_binding_indent
    ; Decl.to_ui let_binding_spacing
    ; Decl.to_ui let_module
    ; Decl.to_ui line_endings
    ; Decl.to_ui margin
    ; Decl.to_ui match_indent
    ; Decl.to_ui match_indent_nested
    ; Decl.to_ui max_indent
    ; Decl.to_ui module_item_spacing
    ; Decl.to_ui nested_match
    ; Decl.to_ui ocp_indent_compat
    ; Decl.to_ui parens_ite
    ; Decl.to_ui parens_tuple
    ; Decl.to_ui parens_tuple_patterns
    ; Decl.to_ui parse_docstrings
    ; Decl.to_ui parse_toplevel_phrases
    ; Decl.to_ui sequence_blank_line
    ; Decl.to_ui sequence_style
    ; Decl.to_ui single_case
    ; Decl.to_ui space_around_arrays
    ; Decl.to_ui space_around_lists
    ; Decl.to_ui space_around_records
    ; Decl.to_ui space_around_variants
    ; Decl.to_ui stritem_extension_indent
    ; Decl.to_ui type_decl
    ; Decl.to_ui type_decl_indent
    ; Decl.to_ui wrap_comments
    ; Decl.to_ui wrap_fun_args ]
end
