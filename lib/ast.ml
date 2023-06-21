open! Base

type t =
  | Program of program
  | Statement of statement
  | Expression of expression
[@@deriving show]

and identifier = { name : string }
(* and operator = { left : int; operator : Token.t; right : expression } *)
and integer = { number : int }
and prefix = { prefix : Token.t; precedence : int; expression : expression }
and infix = { left : expression; operator : Token.t; right : expression }

and expression =
  | Identifier of identifier
  | Integer of integer
  (* | Operator of operator *)
  | Prefix of prefix
  | Infix of infix

and statement =
  | LetStatement of { identifier : identifier; value : expression }
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

let expr_node_prefix t p e =
  Prefix { prefix = t; precedence = p; expression = e }

let show = function
  | Program p -> show_program p
  | Statement s -> show_statement s
  | Expression e -> show_expression e
