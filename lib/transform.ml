type mixed =
  | LS
  | LK
  | LI
  | Var of string
  | L of string * mixed (* *)
  | A of mixed * mixed (* (_ _) *)


let rec to_string (exp : mixed) : string = match exp with
  | Var x -> x
  | LS -> "S"
  | LI -> "I"
  | LK -> "K"
  | L (n, e) -> "λ" ^ n ^ "." ^ (to_string e)
  | A (e1, e2) -> 
    "(" ^ (to_string e1) ^ " " ^ (to_string e2) ^ ")"

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
  A(LS, A(LK, A(LK, Var "x"))), "(S (K (K x)))";  (* Expected result: x *)
  A(LS, A(LK, A(LK, Var "x"))), "(S (K (K x)))";  (* Test another simple example *)
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



let rec transform (exp : mixed) =
  let result = match exp with
    | LK -> LK
    | LS -> LS
    | LI -> LI
    | Var x ->
      Printf.printf "Applying (1)\n";
      let res = Var x in
      Printf.printf "Transformed %s to %s\n" (to_string exp) (to_string res);
      res
    | A (e1, e2) ->
      Printf.printf "Applying (2)\n";
      let res = A (transform e1, transform e2) in
      Printf.printf "Transformed %s to %s\n" (to_string exp) (to_string res);
      res
    | L (n, e) when not (occurs_free n e)->
      Printf.printf "Applying (3)\n";
      let res = A (LK, transform e) in
      Printf.printf "Transformed %s to %s\n" (to_string exp) (to_string res);
      res
    | L (n, Var n') when n = n' ->
      Printf.printf "Applying (4)\n";
      let res = LI in
      Printf.printf "Transformed %s to %s\n" (to_string exp) (to_string res);
      res
    | L (x, L (y, e)) when (occurs_free x e) ->
      Printf.printf "Applying (5)\n";
      let res = transform (L (x, transform (L (y, e)))) in
      Printf.printf "Transformed %s to %s\n" (to_string exp) (to_string res);
      res
    | L (x, A (e1, e2)) when occurs_free x e1 || occurs_free x e2 ->
      Printf.printf "Applying (6)\n";
      let res = A (A (LS, (transform (L (x, e1)))), transform (L (x, e2))) in
      Printf.printf "Transformed %s to %s\n" (to_string exp) (to_string res);
      res
    | _ ->
      failwith "unexpected case"
  in
  result

let main () = 
  let wex = L ("x", L("y", A (Var "y", Var "x"))) in
  let res = transform wex in
  Printf.printf "Wiki example: %s\n" (to_string res);
  run_tests ();
  let ex2 = L("y", A (Var "y", Var "x")) in
  let t2 = transform ex2 in
  Printf.printf "Final 2 : %s\n" (to_string t2);
  let exp = A (A (Var "S", A (Var "K", Var "I")), A (A (Var "S", Var "K"), Var "I")) in
  Printf.printf "Check : %s\n" (to_string exp)
