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

module Error : sig
  type t =
    | Bad_value of string * string
    | Malformed of string
    | Misplaced of string * string
    | Unknown of string * [`Msg of string] option
    | Version_mismatch of {read: string; installed: string}

  val to_string : t -> string
end

module type CONFIG = sig
  type config

  val profile_option_names : string list

  val warn_deprecated :
    config -> Location.t -> ('a, Format.formatter, unit, unit) format4 -> 'a
end

type parsed_from = [`File of Location.t | `Attribute of Location.t]

type updated_from = [`Env | `Commandline | `Parsed of parsed_from]

type typ =
  | Int
  | Bool
  | Range
  | Ocaml_version
  | Choice of string list
  | String

module UI : sig
  type 'config t =
    { names: string list
    ; values: typ
    ; doc: string
    ; update: 'config -> string -> updated_from -> 'config }
end

module type S = sig
  type config

  type 'a t

  type option_store

  type kind = Formatting | Operational

  type deprecated

  type removed

  type status = [`Valid | `Deprecated of deprecated | `Removed of removed]

  type 'a option_decl =
       names:string list
    -> doc:string
    -> kind:kind
    -> ?allow_inline:bool
    -> ?status:[`Valid | `Deprecated of deprecated]
    -> (config -> 'a -> updated_from -> config)
    -> (config -> 'a)
    -> 'a t

  val empty_store : option_store

  val add_option : option_store -> 'a t -> option_store

  val merge_options : option_store -> option_store -> option_store

  val section_name : kind -> status -> string

  val deprecated : since:Version.t -> string -> deprecated

  val removed : since:Version.t -> string -> removed

  module Value : sig
    type 'a t

    val make : ?deprecated:deprecated -> name:string -> 'a -> string -> 'a t
  end

  module Value_removed : sig
    (** Indicate that a configuration value has been removed in an
        ocamlformat release. A message indicating how to migrate will be
        displayed. *)
    type t

    val make : name:string -> since:Version.t -> msg:string -> t
    (** [name] is the configuration value that was removed in version
        [since]. [msg] explains how to get the former behaviour. *)

    val make_list :
      names:string list -> since:Version.t -> msg:string -> t list
    (** Shorthand for [mk] when [since] and [msg] are shared. This can be
        used when multiple values are removed at the same time. *)
  end

  val choice :
       all:'a Value.t list
    -> ?removed_values:Value_removed.t list
    -> 'a option_decl

  val flag : default:bool -> bool option_decl

  val int : default:int -> docv:string -> int option_decl

  val range :
       default:(string -> Range.t)
    -> docv:string
    -> (string -> Range.t) option_decl

  (* val string : default:string -> docv:string -> string option_decl *)

  val ocaml_version : default:Ocaml_version.t -> Ocaml_version.t option_decl

  val any :
       'a Cmdliner.Arg.conv
    -> values:typ
    -> default:'a
    -> docv:string
    -> 'a option_decl

  val removed_option :
    names:string list -> since:Version.t -> msg:string -> unit t
  (** Declare an option as removed. Using such an option will result in an
      helpful error including [msg] and [since]. *)

  val default : 'a t -> 'a

  val get_value : 'a t -> config -> 'a

  val update_using_cmdline : option_store -> config -> config * option_store

  val update :
       option_store
    -> config:config
    -> from:updated_from
    -> name:string
    -> value:string
    -> inline:bool
    -> (config * option_store, Error.t) Result.t

  val to_ui : 'a t -> config UI.t

  val print_config : option_store -> config -> unit

  val term_of_options : option_store -> (config -> config) Cmdliner.Term.t
end

module Make (C : CONFIG) : S with type config = C.config
