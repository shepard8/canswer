(** Relation names are the schema of the relations. In our case, they specify
  the atttributes composing a relation, and the attributes that are part of
  the key. *)

type name = string
type attribute = string

type t = private {
  name : name;
  attributes : attribute Lset.t;
  key : attribute -> bool;
}

val make : name -> attribute Lset.t -> (attribute -> bool) -> t
(** Creates a relation name, ensuring that the attributes set uses the standard
  equality function. *)

val signature : name -> int -> int -> t
(** [signature name arity keylength] Creates a relation name from a signature
  (an arity and a key length). Attributes names are undefined, but guaranteed
  to be distinct. *)

val fullkey : name -> attribute Lset.t -> t
(** Creates a relation name from an attribute set, and consider all attributes
  are part of the key. *)

val binary : name -> attribute -> attribute -> t
(** Creates a binary relation in which one attribute is part of the key. *)

val key : t -> attribute Lset.t
(** Returns the set of attributes composing the key. *)

val ( = ) : t -> t -> bool
(** Are two relation name equal ? (same name, same attributes, same key) *)
