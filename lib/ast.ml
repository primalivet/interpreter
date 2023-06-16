open! Base

type t =
  | Program of program
  | Statement of statement
  | Expression of expression
[@@deriving show]

and identifier = { name : string }
and operator = { left : int; operator : Token.t; right : expression }
and integer = { number : int }

and expression =
  | Identifier of identifier
  | Integer of integer
  | Operator of operator

and statement = LetBinding of { identifier : identifier; value : expression }
and program = { statements : statement list }

let node_program ss = Program { statements = ss }
let node_identifier s = Identifier { name = s }
let node_letbinding i e = LetBinding { identifier = i; value = e }
let node_operator t l r = Operator { left = l; operator = t; right = r }
let node_integer i = Integer { number = i }

let show = function
  | Program p -> show_program p
  | Statement s -> show_statement s
  | Expression e -> show_expression e
