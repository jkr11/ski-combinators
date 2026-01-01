open Ast

let rec mixed_to_string (exp : mixed) : string = 
  let rec inner is_right exp = 
    match exp with
    | Var x -> x
    | LS -> "S" | LI -> "I" | LK -> "K"
    | L (n, e) -> "λ" ^ n ^"." ^(mixed_to_string e)
    | A (e1, e2) -> 
        let s = (inner false e1) ^ (inner true e2) in
        if is_right then "(" ^ s ^ ")" else s
    in inner false exp 

type token = LAM | DOT | LPAREN | RPAREN | VAR of char

let string_of_token tok = match tok with
  | LAM  -> "Lam"
  | DOT -> "."
  | LPAREN -> "Lparen"
  | RPAREN -> "Rparen"
  | VAR x -> "Var(" ^ String.make 1 x ^ ")"

let tokenize s = 
  let rec loop i acc = 
    if i >= String.length s then List.rev acc
    else match s.[i] with
      | ' ' | '\n' | '\t' | '\r' -> loop (i+1) acc
      | '\\' -> loop (i+1) (LAM :: acc)
      | '(' -> loop (i+1) (LPAREN :: acc)
      | ')' -> loop (i+1) (RPAREN :: acc)
      | '.' -> loop (i+1) (DOT :: acc)
      | c -> loop (i+1) (VAR c :: acc)
  in
  loop 0 []

exception ParseError of string

let rec parse_expr tokens =
  match tokens with
  | LAM :: VAR x :: DOT :: rest ->
      let (body, remaining) = parse_expr rest in
      (L (String.make 1 x, body), remaining)
  | _ -> parse_app tokens

and parse_app tokens =
  let (left, rest) = parse_atom tokens in
  let rec apply_left acc ts =
    try
      let (next, remaining) = parse_atom ts in
      apply_left (A (acc, next)) remaining
    with _ -> (acc, ts) (* No more atoms to apply *)
  in
  apply_left left rest

and parse_atom tokens =
  match tokens with
  | VAR x :: rest -> (Var (String.make 1 x), rest)
  | LPAREN :: rest ->
      let (e, remaining) = parse_expr rest in
      (match remaining with
       | RPAREN :: after -> (e, after)
       | _ -> raise (ParseError "Expected ')'"))
  | _ -> raise (ParseError "Unexpected token")

let compile_and_run str =
  let tokens = tokenize str in
  let (ast, _) = parse_expr tokens in
  Printf.printf "%s" (mixed_to_string ast)

let main () = compile_and_run "λx.λy.x"