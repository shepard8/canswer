type dot = string

val symbol : Symbol.t -> dot
val atom : Atom.t -> dot
val node : ?unattacked : bool -> AttackGraph.node -> dot
val attack_graph : AttackGraph.t -> dot
val split_attack_graph : Conjunctive.t -> dot
val cqafo : CqaFO.t -> dot
val lattice : Expanded.t -> dot

