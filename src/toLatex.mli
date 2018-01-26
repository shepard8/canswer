type t = string

val symbol : Symbol.t -> t
val relation_name : RelationName.t -> t
val substitution : Substitution.t -> t
val atom : Atom.t -> t
val drcf : Drc.formula -> t
val drc : Drc.t -> t
val trcf : Trc.formula -> t
val trc : Trc.t -> t

