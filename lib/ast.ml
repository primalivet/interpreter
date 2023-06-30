open! Base

type t =
  | Program of program
  | Statement of statement
  | Expression of expression
[@@deriving show]

and identifier = { name : string }

(* and operator = { left : int; operator : Token.t; right : expression } *)
and integer = { number : int }

and prefix =
  { prefix : Token.t
  ; expression : expression
  }

and infix =
  { left : expression
  ; operator : Token.t
  ; right : expression
  }

and expression =
  | Identifier of identifier
  | Integer of integer
  (* | Operator of operator *)
  | Prefix of prefix
  | Infix of infix

and statement =
  | LetStatement of
      { identifier : identifier
      ; value : expression
      }
  | ReturnStatement of { value : expression }
  | ExpressionStatement of { value : expression }

and program = { statements : statement list }

let stmnt_node_program ss = Program { statements = ss }
let stmnt_node_return e = ReturnStatement { value = e }
let stmnt_node_let i e = LetStatement { identifier = i; value = e }
let stmnt_node_expression e = ExpressionStatement { value = e }
let expr_node_identifier s = Identifier { name = s }

(* let expr_node_operator t l r = Operator { left = l; operator = t; right = r } *)
let expr_node_integer i = Integer { number = i }
let expr_node_prefix t e = Prefix { prefix = t; expression = e }
let expr_node_infix t lhs rhs = Infix { left = lhs; operator = t; right = rhs }

let show = function
  | Program p -> show_program p
  | Statement s -> show_statement s
  | Expression e -> show_expression e
;;

let rec to_literal_str ast =
  let inner ss ast =
    match ast with
    | Program p -> to_literal_str_prog p :: ss
    | Statement s -> to_literal_str_stmnt s :: ss
    | Expression e -> to_literal_str_expr e :: ss
  in
  inner [] ast |> String.concat ~sep:""

and to_literal_str_stmnt = function
  | LetStatement { identifier; value } ->
    Printf.sprintf "let %s = %s" identifier.name (to_literal_str_expr value)
  | ReturnStatement { value } -> Printf.sprintf "return %s" (to_literal_str_expr value)
  | ExpressionStatement { value } -> to_literal_str_expr value

and to_literal_str_expr = function
  | Identifier { name } -> name
  | Integer { number } -> Int.to_string number
  | Infix { left; operator; right } ->
    Printf.sprintf
      "(%s %s %s)"
      (to_literal_str_expr left)
      (Token.to_literal_str operator)
      (to_literal_str_expr right)
  | Prefix { prefix; expression } ->
    Printf.sprintf "(%s%s)" (Token.to_literal_str prefix) (to_literal_str_expr expression)

and to_literal_str_prog p =
  p.statements |> List.map ~f:to_literal_str_stmnt |> String.concat ~sep:""
;;
