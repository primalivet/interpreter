open! Base

type t = { input : string; pos : int; ch : char option } [@@deriving show]

let step ?(n = 1) l =
  let next_pos = l.pos + n in
  if next_pos >= String.length l.input then { l with pos = next_pos; ch = None }
  else { l with pos = next_pos; ch = Some (String.get l.input next_pos) }

let peek l =
  let next_pos = l.pos + 1 in
  if next_pos >= String.length l.input then None
  else Some (String.get l.input next_pos)

let skip ~pred l =
  let rec inner l =
    match l.ch with None -> l | Some c -> if pred c then inner (step l) else l
  in
  inner l

let take_all ~pred l =
  let rec inner (s, l) =
    match l.ch with
    | None -> (s, l)
    | Some c when pred c -> inner (s ^ Char.to_string c, step l)
    | Some _ -> (s, l)
  in
  inner ("", l)

let eat l =
  l |> skip ~pred:Char.is_whitespace |> fun l ->
  match l.ch with
  | None -> (Token.Eof, step l)
  | Some '+' -> (Token.Plus, step l)
  | Some '-' -> (Token.Minus, step l)
  | Some '*' -> (Token.Asterisk, step l)
  | Some '/' -> (Token.Slash, step l)
  | Some '<' -> (Token.Lt, step l)
  | Some '>' -> (Token.Gt, step l)
  | Some ',' -> (Token.Comma, step l)
  | Some ';' -> (Token.Semicolon, step l)
  | Some '(' -> (Token.Lparen, step l)
  | Some ')' -> (Token.Rparen, step l)
  | Some '{' -> (Token.Lbrace, step l)
  | Some '}' -> (Token.Rbrace, step l)
  | Some '=' -> (
      match peek l with
      | Some '=' -> (Token.Eq, step ~n:2 l)
      | _ -> (Token.Assign, step l))
  | Some '!' -> (
      match peek l with
      | Some '=' -> (Token.NotEq, step ~n:2 l)
      | _ -> (Token.Bang, step l))
  | Some c when Char.is_alpha c ->
      take_all ~pred:Char.is_alpha l |> fun (s, l) -> (Token.try_keyword s, l)
  | Some c when Char.is_digit c ->
      take_all ~pred:Char.is_digit l |> fun (s, l) ->
      (Token.Int (Int.of_string s), l)
  | Some _ -> (Token.Illegal, step l)

let init s =
  if String.length s > 0 then { input = s; pos = 0; ch = Some (String.get s 0) }
  else { input = ""; pos = 0; ch = None }

let collect l =
  let rec inner ts l =
    match l |> eat with
    | Token.Eof, _ -> Token.Eof :: ts
    | t, l -> inner (t :: ts) l
  in
  inner [] l |> List.rev
