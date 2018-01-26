%{
  let i = ref 0
  let gen s =
    i := !i + 1;
    string_of_int (!i)

  let build_relation name k nk =
    let attributes = Lset.(k + nk) in
    let key a = Lset.mem a k in
    RelationName.make name attributes key

  let build_atom name k nk =
    let module S = Substitution in {
      Atom.relation = build_relation name (S.domain k) (S.domain nk);
      Atom.f = fun a -> S.(apply k (apply nk (Symbol.Variable a)))
    }

  let rec from_schema start f = function
    | 0 -> start
    | i -> from_schema (Conjunctive.(start + f i)) f (i - 1)

%}

%token <string> SCHSTRING SYMSTRING
%token <int> INT
%token LPAR RPAR ARROW PIPE AT EOF

%start symbol relation_name substitution atom conjunctive
%type <Symbol.t> symbol
%type <RelationName.t> relation_name
%type <Substitution.t> substitution
%type <Atom.t> atom
%type <Conjunctive.t> conjunctive

%%

conjunctive :
  | boolean EOF { $1 }
  | free PIPE boolean EOF { Lset.fold_right Conjunctive.( @ ) $1 $3 }

free :
  | symbol { Lset.sing $1 }
  | symbol free { Lset.($1 +< $2) }

symbol :
  | SYMSTRING {
    match $1.[0] with
    | 'a' .. 'h' -> Symbol.Constant $1
    | _ -> Symbol.Variable $1
  }

boolean :
  | { Conjunctive.empty }
  | atom boolean { Conjunctive.($2 + $1) }
  | atomschema boolean { from_schema $2 (snd $1) (fst $1) }

atom :
  | SCHSTRING LPAR maps PIPE maps RPAR { build_atom $1 $3 $5 }
  | SCHSTRING LPAR maps PIPE RPAR { build_atom $1 $3 Substitution.id }
  | SCHSTRING LPAR PIPE maps RPAR { build_atom $1 Substitution.id $4 }
  | SCHSTRING LPAR PIPE RPAR { build_atom $1 Substitution.id Substitution.id }

maps :
  | map { Substitution.(id + $1) }
  | map maps { Substitution.($2 + $1) }

map :
  | symbol { Substitution.(gen() * $1) }
  | SCHSTRING symbol { Substitution.($1 * $2) }

substitution :
  | singlesubst { Substitution.(id + $1) }
  | singlesubst substitution { Substitution.($2 + $1) }

singlesubst :
  | variable ARROW symbol { Substitution.($1 * $3) }

variable :
  | symbol {
    match $1 with
    | Symbol.Variable x -> x
    | _ -> failwith "A substitution can not map a constant to a symbol."
  }

atomschema :
  | SCHSTRING AT INT LPAR mapschemas PIPE mapschemas RPAR {
    ($3, fun i -> build_atom ($1 ^ string_of_int i) ($5 i) ($7 i))
  }
  | SCHSTRING AT INT LPAR PIPE mapschemas RPAR {
    ($3, fun i -> build_atom ($1 ^ string_of_int i) Substitution.id ($6 i))
  }
  | SCHSTRING AT INT LPAR mapschemas PIPE RPAR {
    ($3, fun i -> build_atom ($1 ^ string_of_int i) ($5 i) Substitution.id)
  }

mapschemas :
  | mapschema { fun i -> Substitution.(id + $1 i) }
  | mapschema mapschemas { fun i -> Substitution.($2 i + $1 i) }

mapschema :
  | symbol { fun _ -> Substitution.(gen() * $1) }
  | SCHSTRING symbol { fun _ -> Substitution.($1 * $2) }
  | SYMSTRING AT INT { fun i ->
      let v = Symbol.Variable ($1 ^ string_of_int (i + $3)) in
      Substitution.(gen() * v)
  }
  | SCHSTRING SYMSTRING AT INT { fun i ->
      let v = Symbol.Variable ($2 ^ string_of_int (i + $4)) in
      Substitution.($1 * v)
  }

relation_name :
  | SCHSTRING LPAR attrs PIPE attrs RPAR { (build_relation $1 $3 $5) }
  | SCHSTRING LPAR attrs PIPE RPAR { (build_relation $1 $3 Lset.empty) }
  | SCHSTRING LPAR PIPE attrs RPAR { (build_relation $1 Lset.empty $4) }

attrs :
  | SCHSTRING { Lset.sing $1 }
  | SCHSTRING attrs { Lset.($1 +< $2) }

%%

