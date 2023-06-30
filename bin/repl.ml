open Base
open Stdio
open Interpreter

let rec loop () =
  match In_channel.input_line stdin with
  | Some "quit" -> ()
  | Some s when String.is_prefix ~prefix:"tokens#" s ->
    s
    |> String.chop_prefix_if_exists ~prefix:"tokens#"
    |> Lexer.init
    |> Lexer.collect
    |> List.iter ~f:(fun t ->
         match t with
         | Token.Eof -> ()
         | t -> print_endline (Token.show t));
    loop ()
  | Some s when String.is_prefix ~prefix:"ast#" s ->
    (try
       s
       |> String.chop_prefix_if_exists ~prefix:"ast#"
       |> Parser.parse
       |> Ast.show
       |> print_endline;
       loop ()
     with
     | e ->
       print_endline (Exn.to_string e);
       loop ())
  | Some s when String.is_prefix ~prefix:"string#" s ->
    (try
       s
       |> String.chop_prefix_if_exists ~prefix:"string#"
       |> Parser.parse
       |> Ast.to_literal_str
       |> print_endline;
       loop ()
     with
     | e ->
       print_endline (Exn.to_string e);
       loop ())
  | Some s ->
    (try
       s |> Parser.parse |> Ast.show |> print_endline;
       loop ()
     with
     | e ->
       print_endline (Exn.to_string e);
       loop ())
  | None -> failwith "Crashed"
;;

let () =
  print_endline "";
  print_endline "Welcome to the REPL!";
  print_endline "Use prefix 'ast#'    to show the actual abstract syntax tree";
  print_endline "Use prefix 'tokens#' to show interpeted tokens";
  print_endline "Use prefix 'string#' to show Ast as a string with added parentaces for precedence";
  print_endline "Type \"quit\" to exit.";
  print_endline "---";
  loop ()
;;
