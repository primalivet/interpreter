open! Base

type t =
  | Illegal
  | Eof
  (* Identifiers and literals *)
  | Ident of string
  | Int of int
  | String of string
  (* Operators *)
  | Assign
  | Plus
  | Minus
  | Bang
  | Asterisk
  | Slash
  | Lt
  | Gt
  | Eq
  | NotEq
  (* Delimiters *)
  | Comma
  | Semicolon
  | Lparen
  | Rparen
  | Lbrace
  | Rbrace
  (* Keywords *)
  | Function
  | Let
  | If
  | Else
  | Return
  | True
  | False
  [@@deriving show]

let try_keyword = function
  | "let" -> Let
  | "fn" -> Function
  | "if" -> If
  | "else" -> Else
  | "return" -> Return
  | "true" -> True
  | "false" -> False
  | s -> Ident s
