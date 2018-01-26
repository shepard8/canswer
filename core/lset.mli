(** Sets using an underlying list datatype.

  We do not recommend to use this module for sets intended to be large.

  In order for sets to remain sets, the ['a] type of any ['a t] must be
  immutable, for otherwise the data can be altered afterwards set creation and
  make the list underlying the set contain duplicates, in particular breaking
  the [size] function. The _oasis configuration file enforces the use of the
  -safe-string compilation option, hence [string t] can be used safely. In
  addition, types defined in this library are all immutable. *)

type 'a eq = 'a -> 'a -> bool

type 'a t = private {
  l : 'a list;
  eq : 'a eq;
}

(** {2 Set creation} *)

val empty : 'a t
(** The empty set with the basic equality test [( = )]. *)

val empty_eq : 'a eq -> 'a t
(** Creates an empty set with a special equality test. *)

val ( +> ) : 'a t -> 'a -> 'a t
(** [x +> s] returns the union of [s] and the singleton [x]. *)

val ( +< ) : 'a -> 'a t -> 'a t
(** [x ,, s] returns the union of the singleton [x] and [s]. *)

val init : ?eq:'a eq -> (int -> 'a) -> int -> 'a t
(** [init f n] creates a set [{f 0, ..., f (n-1)}].

  @throws Invalid_argument "negative arity" if [n < 0]. *)

val from_list : ?eq:'a eq -> 'a list -> 'a t
(** [from_list l] transforms a list into a set. *)

val sing : ?eq:'a eq -> 'a -> 'a t
(** [sing x] creates the singleton [x +> empty]. *)

val pair : ?eq:'a eq -> 'a -> 'a -> 'a t
(** [pair x y] creates the pair [x +> y +> empty]. Note that this set may
  contain only one element. *)


(** {2 Set inspection} *)

val mem : 'a -> 'a t -> bool
(** [mem x s] checks if [x] occurs in [s] (in the sense of [s.eq]). *)

val size : 'a t -> int
(** [size s] returns the number of elements in [s]. *)

val ( <= ) : 'a t -> 'a t -> bool
(** [s <= t] checks if [s] is included to [t].

  The result is undefined if [s.eq <> t.eq]. *)

val ( = ) : 'a t -> 'a t -> bool
(** Checks whether two sets are equal (in the sense [s <= t && t <= s]). *)

val pop : 'a t -> ('a * 'a t) option
(** [pop s] returns a pair (x, s') in which [sing x + s' = s] but [!(s' = s)]
  ([+] is defined below). *)


(** {2 Set operations} *)

val ( + ) : 'a t -> 'a t -> 'a t
(** [s + t] returns the union of [s] and [t].

  The result is undefined if [s.eq <> t.eq]. *)

val ( ^ ) : 'a t -> 'a t -> 'a t
(** [s ^ t] returns the intersection of [s] and [t].

 * The result is undefined if [s.eq <> t.eq]. *)

val ( - ) : 'a t -> 'a t -> 'a t
(** [s - t] difference between [s] and [t].

  The reselt is undefined if [s.eq <> t.eq]. *)

val merge : ('a -> 'a -> bool) -> ('a -> 'a -> 'a) -> 'a t -> 'a t
(** [merge frel fmerge] merges elements of a set according to a function of
  relativity [frel] and a merging function [fmerge]. The relativity function
  does not need to be transitive. The resulting set is a minimaly sized set of
  related merged elements. *)

val rev : 'a t -> 'a t
(** [rev s] reverses the order of the elements in the (therefore ordered) set.
 * Notice that the operations in this module preserve the order of elements. *)


(** {2 Functional} *)

val map : ?eq:'b eq -> ('a -> 'b) -> 'a t -> 'b t
(** [map f s] maps every member of [s] using [f] in a new set. *)

val filter : ('a -> bool) -> 'a t -> 'a t
(** [filter f s] removes any element not satisfying [f] from [s]. *)

val mapfilter : ?eq:'b eq -> ('a -> 'b option) -> 'a t -> 'b t
(** [mapfilter f s] applies [f] to each element of [s], then removes any [None]
  element from the resulting set. *)

val mapmerge : ?eq:'b eq -> ('a -> 'b t) -> 'a t -> 'b t
(** [mapmerge f s] applies [f] to each element of [s], then merges the
  resulting sets. Sets of sets are often only meant to be merged. This
  function avoids the easy error of creating sets of sets without the correct
  equality operator. *)

val mapconcat : string -> ('a -> string) -> 'a t -> string
(** [mapconcat sep f s] applies [f] to each element of [s], then concats the
  elements obtained using [sep] as a separator. The duplicates are _not_
  removed. *)

val concat : string -> string t -> string
(** [concat sep s] is synonym for [mapconcat sep (fun x -> x) s]. *)

val fold_left : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b
val fold_right : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b
val iter : ('a -> unit) -> 'a t -> unit
val exists : ('a -> bool) -> 'a t -> bool
val for_all : ('a -> bool) -> 'a t -> bool

