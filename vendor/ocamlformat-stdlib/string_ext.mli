include module type of StdLabels.String
val (=) : t -> t -> bool

val (<>) : t -> t -> bool

val starts_with_whitespace : string -> bool
(** [starts_with_whitespace s] holds if [s] is non empty and starts with a
    whitespace character. *)

val ends_with_whitespace : string -> bool
(** [ends_with_whitespace s] holds if [s] is non empty and ends with a
    whitespace character. *)

val split_on_chars : on:char list -> t -> t list

val is_empty : t -> bool

(**[rstrip ?drop s] returns a string with consecutive chars satisfying [drop] 
   (by default [Char.is_whitespace]) stripped from the end of s.*)
val rstrip : ?drop:(char -> bool) -> t -> t


(**[lstrip ?drop s] returns a string with consecutive chars satisfying [drop] 
   (by default [Char.is_whitespace]) stripped from the start of s.*)
   val lstrip : ?drop:(char -> bool) -> t -> t

   (**[strip ?drop s] returns a string with consecutive chars satisfying [drop] 
   (by default [Char.is_whitespace]) stripped from the start and end of s.*)
val strip : ?drop:(char -> bool) -> t -> t
val split_lines : t -> t list


val filter : f:(char -> bool) -> t -> t

val chop_prefix : prefix:t ->  t -> t option

val chop_suffix : suffix:t ->  t -> t option


val chop_prefix_exn : prefix:t ->  t -> t

val chop_suffix_exn : suffix:t ->  t -> t

val lfindi : ?pos:int -> t -> f:(int -> char -> bool) -> int option

(** [drop_suffix s n] drops the longest suffix of [s] of length less than or equal to
    [n]. *)
    val drop_suffix : t -> int -> t

    (** [drop_prefix s n] drops the longest prefix of [s] of length less than or equal to
        [n]. *)
    val drop_prefix : t -> int -> t