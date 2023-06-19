include module type of StdLabels.List

val partition_map :
  'a list -> f:('a -> ('b, 'c) Either.t) -> 'b list * 'c list
(** [partition_map t ~f] partitions [t] according to [f].

    @since base.v0.14.0 *)

val max_elt : compare:('a -> 'a -> int) -> 'a t -> 'a option

val drop_while : f:('a -> bool) -> 'a t -> 'a t

val is_empty : 'a t -> bool

val last : 'a t -> 'a option

val last_exn : 'a t -> 'a

val split_n : 'a t -> int -> 'a t * 'a t


(** Returns the given list with consecutive duplicates removed.  The relative order of the
    other elements is unaffected.  The element kept from a run of duplicates is determined
    by [which_to_keep]. *)
    val remove_consecutive_duplicates
    :  ?which_to_keep:[ `First | `Last ] (** default = `Last *)
    -> 'a t
    -> equal:(('a -> 'a -> bool)[@local])
    -> 'a t

    (** [group l ~break] returns a list of lists (i.e., groups) whose concatenation is equal
    to the original list.  Each group is broken where [break] returns true on a pair of
    successive elements.

    Example:

    {[
      group ~break:(<>) ['M';'i';'s';'s';'i';'s';'s';'i';'p';'p';'i'] ->

      [['M'];['i'];['s';'s'];['i'];['s';'s'];['i'];['p';'p'];['i']] ]} *)
val group : 'a t -> break:(('a -> 'a -> bool)[@local]) -> 'a t t

(** This is just like [group], except that you get the index in the original list of the
    current element along with the two elements.

    Example, group the chars of ["Mississippi"] into triples:

    {[
      groupi ~break:(fun i _ _ -> i mod 3 = 0)
        ['M';'i';'s';'s';'i';'s';'s';'i';'p';'p';'i'] ->

      [['M'; 'i'; 's']; ['s'; 'i'; 's']; ['s'; 'i'; 'p']; ['p'; 'i']] ]}
*)
val groupi : 'a t -> break:((int -> 'a -> 'a -> bool)[@local]) -> 'a t t

val zip : 'a t -> 'b t -> ('a * 'b) t option