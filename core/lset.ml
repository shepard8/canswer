type 'a eq = 'a -> 'a -> bool
type 'a t = { l : 'a list; eq : 'a eq }

let empty_eq eq = { l = []; eq }
let empty = { l = []; eq = (fun a b -> a = b) }
let empty_opt = function
  | None -> empty
  | Some eq -> empty_eq eq

let mem x s = List.exists (s.eq x) s.l

let ( +< ) a s = if mem a s then s else { s with l = a :: s.l }
let ( +> ) s a = if mem a s then s else { s with l = s.l @ [a] }
let from_list ?eq l = List.fold_left ( +> ) (empty_opt eq) l
let rec init ?eq f = function
  | 0 -> empty_opt eq
  | i when i > 0 ->
      let s = init ?eq f (i - 1) in
      let x = f (i - 1) in
      if mem x s then s else s +> x
  | _ -> raise (Invalid_argument "Negative arity")
let sing ?eq x = empty_opt eq +> x
let pair ?eq x y = empty_opt eq +> x +> y

let map ?eq f s = from_list ?eq (List.map f s.l)
let filter f s = { s with l = List.filter f s.l }
let mapfilter ?eq f s = List.fold_left
    (fun acc x -> match f x with None -> acc | Some x -> acc +> x)
    (empty_opt eq) s.l

(* These functions are not tested *)
let fold_left f x0 s = List.fold_left f x0 s.l
let fold_right f s x0 = List.fold_right f s.l x0
let iter f s = List.iter f s.l
let exists p s = List.exists p s.l
let for_all p s = List.for_all p s.l

let size s = List.length s.l (* This suffices as the t type is private and
every list constructed by this module has no duplicates *)
let ( <= ) s t = for_all (fun x -> mem x t) s
let ( = ) s t = s <= t && t <= s
let pop { l; eq } = match l with
| [] -> None
| h :: t -> Some (h, {l = t; eq})

let ( + ) s t = fold_left ( +> ) s t
let ( ^ ) s t = filter (fun x -> mem x t) s
let ( - ) s t = filter (fun x -> not (mem x t)) s

let mapmerge ?eq f s =
  let sets = map ~eq:( = ) f s in
  fold_left ( + ) (empty_opt eq) sets

let rev s = { s with l = List.rev s.l }

let merge frel fmerge s =
  let rec aux acc work =
    match pop work with
    | None -> acc
    | Some (e, s) ->
        let elts_in = filter (frel e) s in
        if elts_in = empty then aux (e +< acc) s
        else aux acc ((fold_left fmerge e elts_in) +< (s - elts_in))
  in aux (empty_eq s.eq) s

let mapconcat sep f s = match pop s with
  | None -> ""
  | Some (h, t) ->
    let b = Buffer.create 80 in
    Buffer.add_string b (f h);
    let rec aux = function
      | [] -> Buffer.contents b
      | h :: t ->
        Buffer.add_string b sep;
        Buffer.add_string b (f h);
        aux t
    in aux t.l

let concat sep s = mapconcat sep (fun x -> x) s

