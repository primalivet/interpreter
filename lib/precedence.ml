open! Base

type t =
  | Lowest
  | Equals
  | LessGreater
  | Sum
  | Product
  | Prefix
  | Call
[@@deriving show]

let to_int = function
  | Lowest -> 1
  | Equals -> 2
  | LessGreater -> 3
  | Sum -> 4
  | Product -> 5
  | Prefix -> 6
  | Call -> 7
;;

let of_token = function
  | Token.Eq -> Equals
  | Token.NotEq -> Equals
  | Token.Lt -> LessGreater
  | Token.Gt -> LessGreater
  | Token.Plus -> Sum
  | Token.Minus -> Sum
  | Token.Slash -> Product
  | Token.Asterisk -> Product
  | _ -> Lowest
;;

let lt t1 t2 = to_int t2 < to_int t1
let gt t1 t2 = to_int t2 > to_int t1
