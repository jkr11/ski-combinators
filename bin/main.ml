open Ski.Transform
(*open Ski.Ast*)
(*open Ski.Parser*)
let () = main ()

(*let () = 
  let l_true = L ("x", L ("y", Var "x")) in
  let l_false = L ("x", L ("y", Var "y")) in

  let l_iszero = L ("n", A (A (Var "n", L ("x", l_false)), l_true)) in
  let l_0 = L ("f", L ("x", Var "x")) in
  let l_1 = L ("f", L ("x", A (Var "f", Var "x"))) in

  let l_mult = L ("m", L ("n", L ("f", A (Var "m", A (Var "n", Var "f"))))) in

  let l_pred = 
    L ("n", L ("f", L ("x", 
      A (A (A (Var "n", 
        L ("g", L ("h", A (Var "h", A (Var "g", Var "f"))))),
        L ("u", Var "x")),
        L ("u", Var "u"))
    ))) in

  let l_y = 
    L ("f", 
      A (L ("x", A (Var "f", A (Var "x", Var "x"))),
         L ("x", A (Var "f", A (Var "x", Var "x"))))
    ) in

  let l_fac_gen = 
    L ("f", L ("n", 
      A (A (A (l_iszero, Var "n"), 
          l_1), (* Base case: if 0 then 1 *)
          A (A (l_mult, Var "n"), 
             A (Var "f", A(l_pred, Var "n")))) (* Recursive case: n * f(n-1) *)
    )) in

  let l_fac = A (l_y, l_fac_gen) in

  let mixed_ski_fac = transform l_fac in

  let pure_cl_fac = mixed_to_comb mixed_ski_fac in

  let run_factorial n_val =
    let mk_church i =
      if i = 0 then mixed_to_comb (transform l_0)
      else 
        let succ = L("n", L("f", L("x", A(Var "f", A(A(Var "n", Var "f"), Var "x"))))) in
        mixed_to_comb (transform (A(succ, transform (L("f", L("x", 
          let rec loop j = if j = 0 then Var "x" else A(Var "f", loop (j-1)) in loop i
        ))))))
    in
    let input = mk_church n_val in
    let expr = App (pure_cl_fac, input) in
    evaluate expr
  in

  let result = run_factorial 2 in (* 2! = 2 *)
  let expected = mixed_to_comb (transform (L("f", L("x", A(Var "f", A(Var "f", Var "x")))))) in
  let z = CVar "__z" in
  let rz = App (result, z) in
  let ez = App (expected, z) in
  if are_equal rz ez 50000 then
    print_endline "Factorial computation successful!"
  else
    print_endline "Result did not match expected Church Numeral."*)