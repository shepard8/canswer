type collapsed = private Expanded.t

val collapse : Expanded.t -> collapsed

val remove_transitive_edges : collapsed -> Expanded.t

val make : Expanded.t -> Expanded.t
(** [make e] is [remove_transitive_edges (collapse m)]. *)

