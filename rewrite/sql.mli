type t = string

val from_trc : Trc.t -> t

val conjunctive : Conjunctive.t -> t
val rewrite : ?group : bool -> ?split : bool -> Conjunctive.t -> t
