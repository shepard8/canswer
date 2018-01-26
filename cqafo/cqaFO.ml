type t = {
  query : Conjunctive.t;
  insourced : Symbol.variable Lset.t;
  outsourced : Symbol.variable Lset.t;
}

let make query = {
  query;
  insourced = Conjunctive.vars query;
  outsourced = Lset.empty
}

let freed_query { query; outsourced; _ } =
  Lset.fold_right Conjunctive.( @ ) (Lset.map Symbol.var outsourced) query

let outsource { query; outsourced; insourced } v =
  if Lset.mem v insourced
  then {
    query;
    outsourced = Lset.(v +< outsourced);
    insourced = Lset.(insourced - sing v)
  }
  else { query; outsourced; insourced }

let substitute { query; insourced; outsourced } x y =
  let x, y = min x y, max x y in {
    query = Conjunctive.substitute Substitution.(id + x * Symbol.var y) query;
    insourced = Lset.(insourced - sing x);
    outsourced = Lset.(outsourced - sing x);
  }

let ( = ) c c' =
  Conjunctive.(c.query = c'.query) && Lset.(c.outsourced = c'.outsourced)

