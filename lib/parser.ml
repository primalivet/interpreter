open! Base

type t = { lexer : Lexer.t; current : Token.t; next : Token.t }
[@@deriving show]

type precedence =
  | Lowest
  | Equals
  | LessGreater
  | Sum
  | Product
  | Prefix
  | Call
[@@deriving show]

let what_precedence precedence =
  match precedence with
  | Token.Eq -> Equals
  | Token.NotEq -> Equals
  | Token.Lt -> LessGreater
  | Token.Gt -> LessGreater
  | Token.Plus -> Sum
  | Token.Minus -> Sum
  | Token.Slash -> Product
  | Token.Asterisk -> Product
  | _ -> Lowest

let precedence_num precedence =
  match precedence with
  | Lowest -> 1
  | Equals -> 2
  | LessGreater -> 3
  | Sum -> 4
  | Product -> 5
  | Prefix -> 6
  | Call -> 7

type statement_result = (Ast.statement, string) Result.t
type expression_result = (Ast.expression, string) Result.t
type infix_parser_fn = t -> expression_result * t
type prefix_parser_fn = t -> expression_result * t

let init s =
  let l = Lexer.init s in
  let current, l1 = l |> Lexer.eat in
  let next, lexer = l1 |> Lexer.eat in
  { lexer; current; next }

let step p =
  Lexer.eat p.lexer |> fun (t, l) -> { lexer = l; current = p.next; next = t }

let rec step_until t p =
  match p.current with
  | current when Token.equal current t -> p
  | _ -> step_until t (step p)

let step_if t p = if Token.equal p.current t then step p else p

let step_expect t p =
  if Token.is_token t p.current then Result.Ok (step p)
  else
    Result.Error
      (Printf.sprintf "Expected current token to be %s. But got: %s"
         (Token.show p.current) (Token.show t))

let parse_identifier p =
  match p.current with
  | Token.Ident s -> (Result.Ok (Ast.expr_node_identifier s), p)
  | _ -> (Result.Error "Expected identifier", p)

let parse_integerliteral p =
  match p.current with
  | Token.Int i -> (Result.Ok (Ast.expr_node_integer i), p)
  | _ -> (Result.Error "Expected integer", p)

let get_prefix_parser_fn t =
  match t with
  | Token.Ident _ -> Some parse_identifier
  | Token.Int _ -> Some parse_integerliteral
  | _ -> None

let get_infix_parser_fn (_ : Token.t) : infix_parser_fn option = failwith "TODO"

let parse_letstatement p =
  step_expect Token.Let p
  |> Result.bind ~f:(fun p ->
         match (p.current, p.next) with
         | Token.Ident s, Token.Assign ->
             Result.Ok
               (Ast.stmnt_node_let { name = s } (Ast.expr_node_integer 10))
         | c, n ->
             Result.Error
               (Printf.sprintf
                  "Invalid let binding.\n\
                   Expected identifier and equal sign after let keyword. Got: \
                   %s %s"
                  (Token.show c) (Token.show n)))
  (* TODO: parse RHS expression *)
  |> fun statement -> (statement, step_until Token.Semicolon p)

let parse_returnstatement p =
  step_expect Token.Return p
  |> Result.bind ~f:(fun _ ->
         Result.Ok (Ast.stmnt_node_return (Ast.expr_node_integer 10)))
  (* TODO: parse RHS expression *)
  |> fun statement -> (statement, step_until Token.Semicolon p)

let parse_expression (precedence : precedence) p =
  let rec inner (_ : precedence) p =
    match p.current with
    | Token.Ident s -> (Result.Ok (Ast.expr_node_identifier s), p)
    | Token.Int i -> (Result.Ok (Ast.expr_node_integer i), p)
    | Token.Bang -> (
        inner Prefix (step p) |> function
        | Result.Ok e, p ->
            ( Result.Ok
                (Ast.expr_node_prefix Token.Bang (precedence_num Prefix) e),
              p )
        | _ -> failwith "TODO")
    | Token.Minus -> (
        inner Prefix (step p) |> function
        | Result.Ok e, p ->
            ( Result.Ok
                (Ast.expr_node_prefix Token.Minus (precedence_num Prefix) e),
              p )
        | _ -> failwith "TODO")
    | _ ->
        failwith
          (Printf.sprintf "TODO: not yet implemented for token: %s"
             (Token.show p.current))
  in
  inner precedence p

let parse_expressionstatement p =
  parse_expression Lowest p |> fun (expression, p) ->
  expression
  |> Result.map ~f:(fun expression -> Ast.stmnt_node_expression expression)
  |> fun statement -> (statement, step_if Token.Semicolon p)

let parse_statement p =
  match p.current with
  | Token.Let -> parse_letstatement p
  | Token.Return -> parse_returnstatement p
  | _ -> parse_expressionstatement p

let handle_result (presult : statement_result list) =
  List.partition_result presult |> function
  | _, rs when List.length rs <> 0 -> failwith (String.concat ~sep:"\n" rs)
  | ss, _ when List.length ss <> 0 -> Ast.stmnt_node_program (ss |> List.rev)
  | _, _ -> failwith "Empty program"

let parse s =
  let rec inner ((result, p) : statement_result list * t) =
    match (result, p) with
    (* TODO: remove, but skip semicolon for now *)
    | ss, { current = Token.Semicolon; _ } -> inner (ss, step p)
    | ss, { current = Token.Eof; _ } -> (ss, p)
    | ss, p -> parse_statement p |> (fun (s, p) -> (s :: ss, step p)) |> inner
  in
  inner ([], init s) |> fst |> handle_result
