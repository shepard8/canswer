module type EQ = sig
  type t
  val ( = ) : t -> t -> bool
end

module type RC = sig
  type atom
  type variable

  type formula = private
    | Certain of formula
    | True
    | False
    | Atom of atom
    | And of formula Lset.t
    | Or of formula Lset.t
    | Neg of formula
    | Implies of formula * formula
    | Equiv of formula * formula
    | Exists of variable Lset.t * formula
    | Forall of variable Lset.t * formula

  val ( = ) : formula -> formula -> bool
  val vars : formula -> variable Lset.t
  val dtrue : formula
  val dfalse : formula
  val datom : atom -> formula
  val dcertain : formula -> formula
  val dand : formula Lset.t -> formula
  val dor : formula Lset.t -> formula
  val dneg : formula -> formula
  val dimplies : formula -> formula -> formula
  val dequiv : formula -> formula -> formula
  val dexists : variable Lset.t -> formula -> formula
  val dexistsstrict : variable Lset.t -> formula -> formula
  val dforall : variable Lset.t -> formula -> formula
  val dforallstrict : variable Lset.t -> formula -> formula
  val map : (formula -> formula option) -> formula -> formula
end

module Rc = functor (Atom : EQ) -> functor (Variable : EQ) -> struct
  type atom = Atom.t
  type variable = Variable.t

  type formula =
    | Certain of formula
    | True
    | False
    | Atom of atom
    | And of formula Lset.t
    | Or of formula Lset.t
    | Neg of formula
    | Implies of formula * formula
    | Equiv of formula * formula
    | Exists of variable Lset.t * formula
    | Forall of variable Lset.t * formula

  let rec eq a b = match a, b with
  | Certain f, Certain g -> eq f g
  | True, True | False, False -> true
  | Atom f, Atom g -> Atom.(f = g)
  | And s, And t | Or s, Or t -> Lset.(s = t) (* Requires s.eq = t.eq = eq *)
  | Neg f, Neg g -> eq f g
  | Implies (f, g), Implies (h, i) -> eq f h && eq g i
  | Equiv (f, g), Equiv (h, i) -> eq f h && eq g i || eq f i && eq g h
  | Exists (x, f), Exists (y, g) | Forall (x, f), Forall (y, g) ->
      Lset.(x = y) && eq f g
  | _ -> false

  let rec vars = function
    | Certain f -> vars f
    | True | False | Atom _ -> Lset.empty
    | And s | Or s -> Lset.mapmerge vars s
    | Neg f -> vars f
    | Implies (f, g) | Equiv (f, g) -> Lset.(vars f + vars g)
    | Exists (x, f) | Forall (x, f) -> Lset.(x + vars f)

  let rec map t f =
    let default d = match t f with Some x -> x | None -> d in
    match f with
    | True -> default dtrue
    | False -> default dfalse
    | Atom a -> default (datom a)
    | Certain s -> default (dcertain (map t s))
    | And s -> default (dand (Lset.map ~eq (map t) s))
    | Or s -> default (dor (Lset.map ~eq (map t) s))
    | Neg s -> default (dneg (map t s))
    | Implies (s, s') -> default (dimplies (map t s) (map t s'))
    | Equiv (s, s') -> default (dequiv (map t s) (map t s'))
    | Exists (v, s) -> default (dexists v (map t s))
    | Forall (v, s) -> default (dforall v (map t s))
  and dtrue = True
  and dfalse = False
  and datom a = Atom a
  and dcertain s =
    let rec aux = function
      | Certain f -> Some (map aux f)
      | _ -> None
    in match map aux s with
    | True -> dtrue
    | False -> dfalse
    | s -> Certain s
  and dneg = function
  | Neg s -> s
  | True -> dfalse
  | False -> dtrue
  | f -> Neg f
  and dand s =
    let sand = Lset.mapmerge ~eq (function And s' -> s' | _ -> Lset.empty) s in
    let sleft = Lset.filter (function And _ -> false | _ -> true) s in
    let final = Lset.(sand + sleft - sing True) in
    if Lset.mem False s then dfalse
    else if Lset.size final > 1 then And final
    else match Lset.pop final with None -> dtrue | Some (f, _) -> f
  and dor s =
    let sor = Lset.mapmerge ~eq (function Or s' -> s' | _ -> Lset.empty) s in
    let sleft = Lset.filter (function Or _ -> false | _ -> true) s in
    let final = Lset.(sor + sleft - sing False) in
    if Lset.mem True s then dtrue
    else if Lset.size final > 1 then Or final
    else match Lset.pop final with None -> dfalse | Some (f, _) -> f
  and dimplies s t =
    if s = True then t
    else if t = True then dtrue
    else if s = False then dtrue
    else if t = False then dneg s
    else if eq s t then dtrue
    else Implies (s, t)
  and dequiv s t =
    if s = True then t
    else if t = True then s
    else if s = False then dneg t
    else if t = False then dneg s
    else if eq s t then dtrue
    else Equiv (s, t)
  and dexists v = function
    | s when Lset.size v = 0 -> s
    | Exists (v', s) -> dexists Lset.(v + v') s
    | True -> dtrue (* dom is considered non-empty *)
    | False -> dfalse
    | s -> Exists (v, s)
  and dexistsstrict v = function (* no assumption about dom *)
    | s when Lset.size v = 0 -> s
    | True -> Exists (v, dtrue)
    | s -> dexists v s
  and dforall v = function
    | s when Lset.size v = 0 -> s
    | Forall (v', s) -> dforall Lset.(v + v') s
    | True -> dtrue
    | False -> dfalse (* dom is considered non-empty *)
    | s -> Forall (v, s)
  and dforallstrict v = function (* no assumption about dom *)
    | s when Lset.size v = 0 -> s
    | False -> Forall (v, dfalse)
    | s -> dforall v s

  let ( = ) = eq
end
