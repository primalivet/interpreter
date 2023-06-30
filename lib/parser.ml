open! Base

type t =
  { lexer : Lexer.t
  ; current : Token.t
  ; next : Token.t
  }
[@@deriving show]

type statement_result = (Ast.statement, string) Result.t
type expression_result = (Ast.expression, string) Result.t
type infix_parser_fn = t -> expression_result * t
type prefix_parser_fn = t -> expression_result * t

let init s =
  let l = Lexer.init s in
  let current, l1 = l |> Lexer.eat in
  let next, lexer = l1 |> Lexer.eat in
  { lexer; current; next }
;;

let step p = Lexer.eat p.lexer |> fun (t, l) -> { lexer = l; current = p.next; next = t }

let rec step_until t p =
  match p.current with
  | current when Token.equal current t -> p
  | _ -> step_until t (step p)
;;

let step_if t p = if Token.equal p.current t then step p else p

let step_expect t p =
  if Token.is_token t p.current
  then Result.Ok (step p)
  else (
    let msg =
      Printf.sprintf
        "Expected current token to be %s. But got: %s"
        (Token.show p.current)
        (Token.show t)
    in
    Error msg)
;;

let rec prefix_fn t =
  match t with
  | Token.Ident _ -> Some parse_identifier
  | Token.Int _ -> Some parse_integer
  | Token.Bang -> Some parse_prefix
  | Token.Minus -> Some parse_prefix
  | _ -> None

and infix_fn t =
  match t with
  | Token.Plus -> Some parse_infix
  | Token.Minus -> Some parse_infix
  | Token.Slash -> Some parse_infix
  | Token.Asterisk -> Some parse_infix
  | Token.Eq -> Some parse_infix
  | Token.NotEq -> Some parse_infix
  | Token.Lt -> Some parse_infix
  | Token.Gt -> Some parse_infix
  | _ -> None

and parse_identifier p =
  match p.current with
  | Token.Ident s -> Result.Ok (Ast.expr_node_identifier s), p
  | _ -> Error "Expected identifier", p

and parse_integer p =
  match p.current with
  | Token.Int i -> Result.Ok (Ast.expr_node_integer i), p
  | _ -> Error "Expected integer", p

and parse_prefix p =
  let current_token = p.current in
  match current_token with
  | Token.Bang | Token.Minus ->
    step p
    |> parse_expression Precedence.Prefix
    |> (function
    | Ok e, p -> Ok (Ast.expr_node_prefix current_token e), p
    | Error r, p -> Error r, p)
  | t -> failwith @@ Printf.sprintf "Unexpected prefix operator: %s" @@ Token.show t

and parse_infix lhs_exp p =
  let precedence = Precedence.of_token p.current in
  let current_token = p.current in
  let stepped = step p in
  stepped
  |> parse_expression precedence
  |> function
  | Ok rhs_exp, p -> Ok (Ast.expr_node_infix current_token lhs_exp rhs_exp), p
  | Error reason, p -> Error reason, p

and parse_expression precedence p =
  let prefix =
    prefix_fn p.current
    |> Option.map ~f:(fun f -> f p)
    |> function
    | Some (Ok expr, p) -> Ok expr, p
    | Some (Error reason, p) -> Error reason, p
    | None -> Error "No matching prefix", p
  in
  match prefix with
  | Ok lhs_exp, p ->
    let rec inner expr p =
      match p with
      | p
        when (not @@ Token.is_token Token.Semicolon p.next)
             && Precedence.lt (Precedence.of_token p.next) precedence ->
        let infix = infix_fn p.next in
        (match infix with
         | Some f ->
           f expr (step p)
           |> (function
           | Ok expr, p -> inner expr p
           | Error r, p -> Error r, p)
         | None -> Ok expr, p)
      | p -> Ok expr, p
    in
    inner lhs_exp p
  | Error reason, p -> Error reason, p

and parse_expression2 (precedence : Precedence.t) p =
  let prefix =
    Some p
    |> Option.apply @@ prefix_fn p.current
    |> function
    | Some (Ok exp, p) -> Ok exp, p
    | Some (Error reason, p) -> Error reason, p
    | None ->
      let msg =
        Printf.sprintf "No matching prefix function for: %s" (Token.show p.current)
      in
      Error msg, p
  in
  match prefix with
  | Ok lhs_exp, p ->
    let infix = infix_fn p.next in
    if (not @@ Token.is_token Token.Semicolon p.next)
       && Precedence.lt (Precedence.of_token p.next) precedence
    then (
      match infix with
      | Some f -> f lhs_exp (step p)
      | None -> Ok lhs_exp, p)
    else Ok lhs_exp, p
  | Error r, p -> Error r, p
;;

let parse_letstatement p =
  step_expect Token.Let p
  |> Result.bind ~f:(fun p ->
       match p.current, p.next with
       | Token.Ident s, Token.Assign ->
         Ok (Ast.stmnt_node_let { name = s } (Ast.expr_node_integer 10))
       | c, n ->
         Error
           (Printf.sprintf
              "Invalid let binding.\n\
               Expected identifier and equal sign after let keyword. Got: %s %s"
              (Token.show c)
              (Token.show n)))
  (* TODO: parse RHS expression *)
  |> fun statement -> statement, step_until Token.Semicolon p
;;

let parse_returnstatement p =
  step_expect Token.Return p
  |> Result.bind ~f:(fun _ -> Ok (Ast.stmnt_node_return (Ast.expr_node_integer 10)))
  (* TODO: parse RHS expression *)
  |> fun statement -> statement, step_until Token.Semicolon p
;;

let parse_expressionstatement p =
  parse_expression Lowest p
  |> fun (expression, p) ->
  expression
  |> Result.map ~f:(fun expression -> Ast.stmnt_node_expression expression)
  |> fun statement -> statement, step_if Token.Semicolon p
;;

let parse_statement p =
  match p.current with
  | Token.Let -> parse_letstatement p
  | Token.Return -> parse_returnstatement p
  | _ -> parse_expressionstatement p
;;

let handle_result (presult : statement_result list) =
  List.partition_result presult
  |> function
  | _, rs when List.length rs <> 0 -> failwith (String.concat ~sep:"\n" rs)
  | ss, _ when List.length ss <> 0 -> Ast.stmnt_node_program (ss |> List.rev)
  | _, _ -> failwith "Empty program"
;;

let parse s =
  let rec inner ((result, p) : statement_result list * t) =
    match result, p with
    (* TODO: remove, but skip semicolon for now *)
    | ss, { current = Token.Semicolon; _ } -> inner (ss, step p)
    | ss, { current = Token.Eof; _ } -> ss, p
    | ss, p -> parse_statement p |> (fun (s, p) -> s :: ss, step p) |> inner
  in
  inner ([], init s) |> fst |> handle_result
;;
