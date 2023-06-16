open! Base

type t = { lexer : Lexer.t; current : Token.t; next : Token.t }
[@@deriving show]

let step p =
  Lexer.eat p.lexer |> fun (t, l) -> { lexer = l; current = p.next; next = t }

let parse_ident (p : t) : (Ast.identifier * t, string) Result.t =
  let stepped = step p in
  match (stepped.current, stepped.next) with
  | Token.Ident s, Token.Assign -> Result.Ok ({ name = s }, stepped |> step)
  | t, _ -> Result.Error ("Expected identifier. Got: " ^ Token.show t)

let rec parse_expression (p : t) : (Ast.expression * t, string) Result.t =
  let stepped = step p in
  match (stepped.current, stepped.next) with
  | Token.Int i, Token.Semicolon -> Result.Ok (Ast.node_integer i, step stepped)
  | Token.Int i, Token.Plus -> (
      stepped |> step |> parse_expression |> function
      | Result.Ok (exp, p) ->
          Result.Ok (Ast.node_operator Token.Plus i exp, p |> step)
      | Result.Error r ->
          failwith ("Expected Operator rhs to be expression. Message: " ^ r))
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
  let rec inner = function
    | ss, { current = Token.Eof; _ } -> ss
    | ss, { current = Token.Let; _ } -> (
        match parse_letbinding p with
        | Result.Ok (s, p) -> inner (s :: ss, step p)
        | Result.Error r -> failwith r)
    | ss, p ->
        failwith
          ("Unexpected token. Got: "
          ^ (ss |> List.map ~f:Ast.show_statement |> String.concat ~sep:"\n")
          ^ show p)
  in
  Ast.node_program (inner ([], p))

let init l =
  l |> Lexer.eat |> fun (t1, l) ->
  l |> Lexer.eat |> fun (t2, l) -> { lexer = l; current = t1; next = t2 }
