type t
type statement_result = (Ast.statement, string) result
type expression_result = (Ast.expression, string) result

val init : string -> t
val step : t -> t
val step_until : Token.t -> t -> t
val step_if : Token.t -> t -> t
val step_expect : Token.t -> t -> (t, string) result
val prefix_fn : Token.t -> (t -> expression_result * t) option
val infix_fn : Token.t -> (Ast.expression -> t -> expression_result * t) option
val parse_identifier : t -> expression_result * t
val parse_integer : t -> expression_result * t
val parse_boolean : t -> expression_result * t
val parse_prefix : t -> expression_result * t
val parse_infix : Ast.expression -> t -> expression_result * t
val parse_expression : Precedence.t -> t -> expression_result * t
val parse_letstatement : t -> statement_result * t
val parse_returnstatement : t -> statement_result * t
val parse_expressionstatement : t -> statement_result * t
val parse_statement : t -> statement_result * t
val handle_result : statement_result list -> Ast.t
val parse : string -> Ast.t
