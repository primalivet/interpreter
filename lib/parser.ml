open! Base

type t = { lexer : Lexer.t; current : Token.t; next : Token.t }
[@@deriving show]

let step p =
  Lexer.eat p.lexer |> fun (t, l) -> { lexer = l; current = p.next; next = t }

let parse_ident (p : t) : (Ast.identifier * t, string) Result.t =
  let stepped = step p in
  match (stepped.current, stepped.next) with
  | Token.Ident s, Token.Assign ->
      Result.Ok (Ast.node_identifier s, stepped |> step)
  | t, _ -> Result.Error ("Expected identifier. Got: " ^ Token.show t)

let rec parse_expression (p : t) : (Ast.expression * t, string) Result.t =
  let stepped = step p in
  match (stepped.current, stepped.next) with
  | Token.Int i, Token.Semicolon -> Result.Ok (Ast.node_integer i, step stepped)
  | Token.Int i, Token.Plus ->
      step stepped |> parse_expression
      |> Result.map_error ~f:(fun reason ->
             "Invalid right hand side of operator. " ^ reason)
      |> Result.map ~f:(fun (exp, p) ->
             (Ast.node_operator Token.Plus i exp, step p))
  | Token.Int i, Token.Minus ->
      step stepped |> parse_expression
      |> Result.map_error ~f:(fun reason ->
             "Invalid right hand side of operator. " ^ reason)
      |> Result.map ~f:(fun (exp, p) ->
             (Ast.node_operator Token.Minus i exp, step p))
  | Token.Int i, Token.Asterisk ->
      step stepped |> parse_expression
      |> Result.map_error ~f:(fun reason ->
             "Invalid right hand side of operator. " ^ reason)
      |> Result.map ~f:(fun (exp, p) ->
             (Ast.node_operator Token.Asterisk i exp, step p))
  | Token.Int i, _ ->
      Result.Error
        ("Expected integer followed by semicolon or operator. Got: "
       ^ Token.show @@ Token.Int i)
  | t, _ -> Result.Error ("Expected integer. Got: " ^ Token.show t)

let parse_letbinding (p : t) : (Ast.statement * t, string) Result.t =
  p |> parse_ident
  |> Result.bind ~f:(fun (id, p) ->
         parse_expression p |> Result.map ~f:(fun (exp, p) -> (id, exp, step p)))
  |> Result.map ~f:(fun (id, exp, p) -> (Ast.node_letbinding id exp, step p))

let parse (p : t) =
  let rec inner (result : (Ast.statement list * t, string) Result.t) =
    match result with
    | Result.Ok (ss, { current = Token.Eof; _ }) -> Result.Ok (ss, p)
    | Result.Ok (ss, { current = Token.Let; _ }) ->
        parse_letbinding p
        |> Result.map ~f:(fun (s, p) -> (s :: ss, step p))
        |> inner
    | Result.Ok (_, p) -> Result.Error ("Unexpected token. Got: " ^ show p)
    | Result.Error r -> Result.Error r
  in
  Result.Ok ([], p) |> inner |> function
  | Result.Ok (ss, _) -> Ast.node_program ss
  | Result.Error r -> failwith r

let init l =
  l |> Lexer.eat |> fun (t1, l) ->
  l |> Lexer.eat |> fun (t2, l) -> { lexer = l; current = t1; next = t2 }
