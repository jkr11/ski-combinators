(* Mixed lambda and SKI expression AST*)
type mixed =
  | LS
  | LK
  | LI
  | Var of string
  | L of string * mixed (* *)
  | A of mixed * mixed (* (_ _) *)

type comb = 
  | CVar of string
  | S
  | K
  | I
  | B
  | C
  | App of comb * comb

let rec mixed_to_comb (expr : mixed) : comb = 
  match expr with
  | Var x -> CVar x
  | LS -> S
  | LK -> K
  | LI -> I
  | A (e1, e2) -> App (mixed_to_comb e1, mixed_to_comb e2)
  | _ -> failwith "Error expression contains lambda"

let comb_to_string (exp : comb) : string =
  let rec inner is_right exp =
    match exp with
    | CVar x -> x
    | S -> "S" | K -> "K" | I -> "I" | B -> "B" | C -> "C"
    | App (e1, e2) ->
        let s = (inner false e1) ^ (inner true e2) in
        if is_right then "(" ^ s ^ ")" else s
  in inner false exp

let rec step (exp : comb) : comb option = 
  match exp with
  | App (I, x) -> Some x
  | App (App (K, x), _) -> Some x
  | App (App (App (S, x), y), z) -> Some (App (App (x, z), App (y, z)))
  | App (App (App (C, f), g), x) -> Some (App (App (f, g), x))
  | App (App (App (B, f), g), x) -> Some (App (f, App (g , x)))
  | App (e1, e2) -> 
    (match step e1 with
      | Some e1' -> Some (App (e1', e2))
      | None ->
        match step e2 with
        | Some e2' -> Some (App (e1, e2'))
        | None -> None)
  | _ -> None

let rec evaluate (exp : comb) : comb =
  match step exp with
  | Some next_exp -> evaluate next_exp
  | None -> exp

let rec reduce fuel exp = 
  if fuel <= 0 then exp
  else match step exp with
    | Some next -> reduce (fuel - 1) next
    | None -> exp

let are_equal (e1 : comb) (e2 : comb) (fuel : int) : bool =
  let nf1 = reduce fuel e1 in
  let nf2 = reduce fuel e2 in
  if nf1 = nf2 then true
  else begin
    Printf.printf "NF1: %s\n NF2: %s\n" (comb_to_string nf1) (comb_to_string nf2);
    false
  end