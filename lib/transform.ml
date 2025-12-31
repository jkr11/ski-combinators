open Ast

let rec to_string' (exp : mixed) : string = match exp with
  | Var x -> x
  | LS -> "S"
  | LI -> "I"
  | LK -> "K"
  | L (n, e) -> "λ" ^ n ^ "." ^ (to_string' e)
  | A (e1, e2) -> 
    "(" ^ (to_string' e1) ^ " " ^ (to_string' e2) ^ ")"

let rec to_string (exp : mixed) : string = 
  let rec inner is_right exp = 
    match exp with
    | Var x -> x
    | LS -> "S" | LI -> "I" | LK -> "K"
    | L (n, e) -> "λ" ^ n ^"." ^(to_string e)
    | A (e1, e2) -> 
        let s = (inner false e1) ^ (inner true e2) in
        if is_right then "(" ^ s ^ ")" else s
    in inner false exp 


let test_cases = [
  Var "x", "x";
  A (Var "x", Var "y"), "(xy)";
  L ("x", Var "x"), "λx.x";
  L ("x", L ("y", Var "y")), "λx.λy.y";
  A (L ("x", Var "x"), Var "y"), "(λx.xy)";
  LS, "S";
  LK, "K";
  LI, "I";
  A (LS, LK), "(SK)";
  L ("x", A (Var "x", Var "y")), "λx.(xy)";
]

let test_cases2 = [
  A(LS, A(LK, A(LK, Var "x"))), "S(K(Kx))";  (* Expected result: x *)
]



let run_tests () =
  List.iter (fun (input, expected) ->
      let result = to_string input in
      if result = expected then
        Printf.printf "Test passed: %s\n" expected
      else
        Printf.printf "Test failed: expected %s, but got %s\n" expected result
    ) test_cases2

let rec occurs_freee (x : string) (exp : mixed) : bool = match exp with
  | Var y -> x = y
  | A (e1, e2) -> occurs_freee x e1 || occurs_freee x e2
  | L (y, e) -> x <> y && occurs_freee x e
  | LS | LK | LI -> false

let occurs_free x (e : mixed) =
  (* Get free variables of an expression *)
  let rec fv (e : mixed) = match e with
    | Var x -> [x]
    | A (e1, e2) -> fv e1 @ fv e2
    | L (x, e) -> List.filter (fun v -> v != x) (fv e)
    | _ -> []
  in
  List.mem x (fv e)



let rec transform (exp : mixed) : mixed =
  match exp with
  | LS | LK | LI -> exp
  | Var x -> Var x
  
  | A (e1, e2) -> A (transform e1, transform e2)
  
  | L (x, e) ->
      begin match e with
      | Var y when x = y -> LI   
      | A (e1, Var y) when x = y && not (occurs_free x e1) -> 
          transform e1  (* eta reduction *)                
      | _ when not (occurs_free x e) ->          
          A (LK, transform e)
      | L (y, e1) ->         
          transform (L (x, transform (L (y, e1))))
      | A (e1, e2) ->
          A (A (LS, transform (L (x, e1))), transform (L (x, e2)))
      | _ -> failwith "Could not transform expression"
      end
let church_encode (n : int) : mixed =
  (* inner builds the f(f(...(f x)...)) part *)
  let rec apply_n count =
    if count <= 0 then 
      Var "x"
    else 
      A (Var "f", apply_n (count - 1))
  in
  L ("f", L ("x", apply_n n))
  
(* succ = λn.λf.λx. f (n f x) *)
let l_succ = 
  L ("n", L ("f", L ("x", 
    A (Var "f", A (A (Var "n", Var "f"), Var "x"))
  )))

(* plus = λm.λn.λf.λx. m f (n f x) *)
let l_plus = 
  L ("m", L ("n", L ("f", L ("x", 
    A (A (Var "m", Var "f"), A (A (Var "n", Var "f"), Var "x"))
  ))))

let run_addition_test () =
  let one = church_encode 1 in
  
  let expr = A (A (l_plus, one), one) in
  
  let ski_mixed = transform expr in
  
  let pure_cl = mixed_to_comb ski_mixed in
  
  let result = evaluate pure_cl in
  
  let expected = mixed_to_comb (transform (church_encode 2)) in
  let z = CVar "__z" in
  if are_equal (App(result,z)) (App(expected,z)) 1000 then
    print_endline "Success: 1 + 1 = 2"
  else
    print_endline "Failure: Result does not match Church 2"

let main () = 
  (*let wex = L ("x", L("y", A (Var "y", Var "x"))) in
  let res = transform wex in
  Printf.printf "Wiki example: %s\n" (to_string res);
  run_tests ();
  let ex2 = L("y", A (Var "y", Var "x")) in
  let t2 = transform ex2 in
  Printf.printf "Final 2 : %s\n" (to_string t2);
  let exp = A (A (Var "S", A (Var "K", Var "I")), A (A (Var "S", Var "K"), Var "I")) in
  Printf.printf "Check : %s\n" (to_string exp);
  Printf.printf "\n";

  let cl_true = LK in
  let cl_false = A (LK, LI) in
  *)
  (* NOT operator: λb. b False True *)
  Printf.printf "%s\n" (to_string (church_encode 2));
  run_addition_test ()