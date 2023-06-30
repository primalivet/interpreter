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
[@@deriving show, eq]

(*
  HINT: we only need to match the Token contructures that takes values,
  deriving eq can do the rest
*)
let is_token t1 t2 =
  match t1, t2 with
  | Ident _, Ident _ -> true
  | Int _, Int _ -> true
  | String _, String _ -> true
  | _, _ -> equal t1 t2
;;

let is_operator = function
  | Assign -> true
  | Plus -> true
  | Minus -> true
  | Bang -> true
  | Asterisk -> true
  | Slash -> true
  | Lt -> true
  | Gt -> true
  | Eq -> true
  | NotEq -> true
  | _ -> false
;;

let try_keyword = function
  | "let" -> Let
  | "fn" -> Function
  | "if" -> If
  | "else" -> Else
  | "return" -> Return
  | "true" -> True
  | "false" -> False
  | s -> Ident s
;;

let to_literal_str = function
  | Illegal -> "ILLEGAL"
  | Eof -> "EOF"
  (* Identifiers and literals *)
  | Ident s -> s
  | Int i -> Int.to_string i
  | String s -> Printf.sprintf "\"%s\"" s
  (* Operators *)
  | Assign -> "="
  | Plus -> "+"
  | Minus -> "-"
  | Bang -> "!"
  | Asterisk -> "*"
  | Slash -> "/"
  | Lt -> "<"
  | Gt -> ">"
  | Eq -> "=="
  | NotEq -> "!="
  (* Delimiters *)
  | Comma -> ","
  | Semicolon -> ";"
  | Lparen -> "("
  | Rparen -> ")"
  | Lbrace -> "{"
  | Rbrace -> "}"
  (* Keywords *)
  | Function -> "fn"
  | Let -> "let"
  | If -> "if"
  | Else -> "else"
  | Return -> "return"
  | True -> "true"
  | False -> "false"
;;
