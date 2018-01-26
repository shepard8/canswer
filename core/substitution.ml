type t = {
  domain : Symbol.variable Lset.t;
  f : Symbol.variable -> Symbol.t;
}

let id = { domain = Lset.empty; f = fun x -> Symbol.Variable x }

let domain theta = theta.domain

let ( * ) x s = (x, s)
let ( + ) t (x, s) =
  if Symbol.Variable x = s then { t with domain = Lset.(t.domain - sing x) }
  else
    { domain = Lset.(x +< t.domain); f = fun y -> if x = y then s else t.f y }

let apply t = function
  | Symbol.Variable y when Lset.(sing y <= t.domain) -> t.f y
  | x -> x

let is_valuation v = Lset.for_all
  (fun x -> match v.f x with Symbol.Constant _ -> true | _ -> false)
  v.domain

let ( = ) theta phi =
  let v x = Symbol.Variable x in
  Lset.(theta.domain = phi.domain) &&
  Lset.for_all (fun x -> apply theta (v x) = apply phi (v x)) theta.domain

