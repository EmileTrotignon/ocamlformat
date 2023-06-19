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

open Migrate_ast

type t

val create_comment : string -> Location.t -> t

val create_docstring : string -> Location.t -> t

val is_docstring : t -> bool

val loc : t -> Location.t

val txt : t -> string

val compare : t -> t -> int

type error =
  { kind: [`Added of t | `Modified of t * t | `Dropped of t]
  ; cmt_kind: [`Comment | `Doc_comment] }

val pp_error : Format.formatter -> error -> unit

type pos = Before | Within | After

type loc = t

module Comparator_no_loc : sig
  type t = loc
  val compare : t -> t -> int

end

val unindent_lines : offset:int -> string list -> string list
(** Detect and remove the baseline indentation of a comment or a code block.
    [offset] is the column number at which the first line starts. *)

    module Set_no_loc : Set.S with type elt = t

module Set : Set.S with type elt = t
module Map_no_loc : Map.S with type key = t

module Map : Map.S with type key = t
