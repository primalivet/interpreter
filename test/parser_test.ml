open Base
open OUnit2
open Interpreter

let make_parser_test name expected input =
  name >:: fun _ -> assert_equal ~printer:Parser.show expected input

let tests =
  "parser tests"
  >::: [
         make_parser_test "init parser fills the current and next token"
           {
             lexer =
               (* { input = "let five"; pos = 8; ch = None } *)
               Lexer.init "let five" |> Lexer.eat |> snd |> Lexer.eat |> snd;
             current = Token.Let;
             next = Token.Ident "five";
           }
           (Lexer.init "let five" |> Parser.init);
       ]

let () = run_test_tt_main tests
