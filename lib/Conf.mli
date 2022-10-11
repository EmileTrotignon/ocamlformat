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

(** Configuration options *)

type from = Config_option.from

type 'a elt = {v: 'a; from: from}

val elt_content : 'a elt -> 'a

val elt_from : 'a elt -> from

val make_elt : 'a -> from -> 'a elt

(** Formatting options *)
type fmt_opts =
  { align_pattern_matching_bar: [`Paren | `Keyword] elt
  ; assignment_operator: [`Begin_line | `End_line] elt
  ; break_before_in: [`Fit_or_vertical | `Auto] elt
  ; break_cases: [`Fit | `Nested | `Toplevel | `Fit_or_vertical | `All] elt
  ; break_collection_expressions: [`Wrap | `Fit_or_vertical] elt
  ; break_colon: [`Before | `After] elt
  ; break_infix: [`Wrap | `Fit_or_vertical] elt
  ; break_infix_before_func: bool elt
  ; break_fun_decl: [`Wrap | `Fit_or_vertical | `Smart] elt
  ; break_fun_sig: [`Wrap | `Fit_or_vertical | `Smart] elt
  ; break_separators: [`Before | `After] elt
  ; break_sequences: bool elt
  ; break_string_literals: [`Auto | `Never] elt
        (** How to potentially break string literals into new lines. *)
  ; break_struct: bool elt
  ; cases_exp_indent: int elt
  ; cases_matching_exp_indent: [`Normal | `Compact] elt
  ; disambiguate_non_breaking_match: bool elt
  ; doc_comments: [`Before | `Before_except_val | `After_when_possible] elt
  ; doc_comments_padding: int elt
  ; doc_comments_tag_only: [`Fit | `Default] elt
  ; dock_collection_brackets: bool elt
  ; exp_grouping: [`Parens | `Preserve] elt
  ; extension_indent: int elt
  ; field_space: [`Tight | `Loose | `Tight_decl] elt
  ; function_indent: int elt
  ; function_indent_nested: [`Always | `Auto | `Never] elt
  ; if_then_else: [`Compact | `Fit_or_vertical | `Keyword_first | `K_R] elt
  ; indent_after_in: int elt
  ; indicate_multiline_delimiters:
      [`No | `Space | `Closing_on_separate_line] elt
  ; indicate_nested_or_patterns: [`Space | `Unsafe_no] elt
  ; infix_precedence: [`Indent | `Parens] elt
  ; leading_nested_match_parens: bool elt
  ; let_and: [`Compact | `Sparse] elt
  ; let_binding_indent: int elt
  ; let_binding_spacing: [`Compact | `Sparse | `Double_semicolon] elt
  ; let_module: [`Compact | `Sparse] elt
  ; line_endings: [`Lf | `Crlf] elt
  ; margin: int elt  (** Format code to fit within [margin] columns. *)
  ; match_indent: int elt
  ; match_indent_nested: [`Always | `Auto | `Never] elt
  ; max_indent: int option elt
  ; module_item_spacing: [`Compact | `Preserve | `Sparse] elt
  ; nested_match: [`Wrap | `Align] elt
  ; ocp_indent_compat: bool elt  (** Try to indent like ocp-indent *)
  ; parens_ite: bool elt
  ; parens_tuple: [`Always | `Multi_line_only] elt
  ; parens_tuple_patterns: [`Always | `Multi_line_only] elt
  ; parse_docstrings: bool elt
  ; parse_toplevel_phrases: bool elt
  ; sequence_blank_line: [`Compact | `Preserve_one] elt
  ; sequence_style: [`Before | `Separator | `Terminator] elt
  ; single_case: [`Compact | `Sparse] elt
  ; space_around_arrays: bool elt
  ; space_around_lists: bool elt
  ; space_around_records: bool elt
  ; space_around_variants: bool elt
  ; stritem_extension_indent: int elt
  ; type_decl: [`Compact | `Sparse] elt
  ; type_decl_indent: int elt
  ; wrap_comments: bool elt  (** Wrap comments at margin. *)
  ; wrap_fun_args: bool elt }

val default_profile : from -> fmt_opts

(** Options changing the tool's behavior *)
type opr_opts =
  { comment_check: bool elt
  ; debug: bool elt  (** Generate debugging output if true. *)
  ; disable: bool elt
  ; margin_check: bool elt
        (** Check whether the formatted output exceeds the margin. *)
  ; max_iters: int elt
        (** Fail if output of formatting does not stabilize within
            [max_iters] iterations. *)
  ; ocaml_version: Ocaml_version.t elt
        (** Version of OCaml syntax of the output. *)
  ; quiet: bool elt
  ; range: (string -> Range.t) elt
  ; disable_conf_attrs: bool elt
  ; version_check: bool elt }

type t = {fmt_opts: fmt_opts; opr_opts: opr_opts}

val default : t

val update : ?quiet:bool -> t -> Parsetree.attribute -> t
(** [update ?quiet c a] updates configuration [c] after reading attribute
    [a]. [quiet] is false by default. *)

val update_value :
  t -> name:string -> value:string -> (t, Config_option.Error.t) Result.t

val update_state : t -> [`Enable | `Disable] -> t

val parse_line :
     t
  -> ?version_check:bool
  -> ?disable_conf_attrs:bool
  -> from:[< `Attribute of Warnings.loc | `File of Warnings.loc]
  -> string
  -> (t, Config_option.Error.t) Result.t

val print_config : t -> unit

val profile_option_names : string list

val collect_warnings : (unit -> t) -> t * (unit -> unit)

val warn :
  loc:Warnings.loc -> ('a, Format.formatter, unit, unit) format4 -> 'a

val docs : string

module UI : sig
  val profile : t Config_option.UI.t

  val fmt_opts : t Config_option.UI.t list

  val opr_opts : t Config_option.UI.t list
end

module C : Config_option.S with type config = t

module Operational : sig
  val update : f:(opr_opts -> opr_opts) -> t -> t

  val comment_check : bool C.t

  val debug : bool C.t

  val disable : bool C.t

  val margin_check : bool C.t

  val max_iters : int C.t

  val ocaml_version : Ocaml_version.t C.t

  val quiet : bool C.t

  val range : (string -> Range.t) C.t

  val disable_conf_attrs : bool C.t
end
