open Base
open OUnit2
open Interpreter

let make_lexer_collect_test name expected input =
  name
  >:: fun _ ->
  assert_equal
    ~printer:(fun ts -> ts |> List.map ~f:Token.show |> String.concat ~sep:" ")
    expected
    (input |> Lexer.init |> Lexer.collect)
;;

let make_lexer_result_test name expected input =
  name
  >:: fun _ ->
  assert_equal
    ~printer:(fun (t, l) -> "(" ^ Token.show t ^ ", " ^ Lexer.show l ^ ")")
    expected
    input
;;

let tests =
  "lexer tests"
  >::: [ make_lexer_result_test
           "eat one token"
           (Token.Plus, { input = "+"; pos = 1; ch = None })
           ("+" |> Lexer.init |> Lexer.eat)
       ; make_lexer_result_test
           "eat two tokens"
           (Token.Eof, { input = "+"; pos = 2; ch = None })
           ("+" |> Lexer.init |> Lexer.eat |> snd |> Lexer.eat)
       ; make_lexer_result_test
           "eat tree tokens"
           (Token.Eof, { input = "+"; pos = 3; ch = None })
           ("+" |> Lexer.init |> Lexer.eat |> snd |> Lexer.eat |> snd |> Lexer.eat)
       ; make_lexer_collect_test
           "two-symbol operators for (==)"
           [ Token.Assign; Token.Eq; Token.Eq; Token.Assign; Token.Eof ]
           "= == ==="
       ; make_lexer_collect_test
           "two-symbol operators for (!=)"
           [ Token.Bang; Token.NotEq; Token.NotEq; Token.Assign; Token.Eof ]
           "! != !=="
       ; make_lexer_collect_test
           "parses book example program (ch1)"
           [ Token.Let
           ; Token.Ident "five"
           ; Token.Assign
           ; Token.Int 5
           ; Token.Semicolon
           ; Token.Let
           ; Token.Ident "ten"
           ; Token.Assign
           ; Token.Int 10
           ; Token.Semicolon
           ; Token.Let
           ; Token.Ident "add"
           ; Token.Assign
           ; Token.Function
           ; Token.Lparen
           ; Token.Ident "x"
           ; Token.Comma
           ; Token.Ident "y"
           ; Token.Rparen
           ; Token.Lbrace
           ; Token.Ident "x"
           ; Token.Plus
           ; Token.Ident "y"
           ; Token.Semicolon
           ; Token.Rbrace
           ; Token.Semicolon
           ; Token.Let
           ; Token.Ident "result"
           ; Token.Assign
           ; Token.Ident "add"
           ; Token.Lparen
           ; Token.Ident "five"
           ; Token.Comma
           ; Token.Ident "ten"
           ; Token.Rparen
           ; Token.Semicolon
           ; Token.Bang
           ; Token.Minus
           ; Token.Slash
           ; Token.Asterisk
           ; Token.Int 5
           ; Token.Semicolon
           ; Token.Int 5
           ; Token.Lt
           ; Token.Int 10
           ; Token.Gt
           ; Token.Int 5
           ; Token.Semicolon
           ; Token.If
           ; Token.Lparen
           ; Token.Int 5
           ; Token.Lt
           ; Token.Int 10
           ; Token.Rparen
           ; Token.Lbrace
           ; Token.Return
           ; Token.True
           ; Token.Semicolon
           ; Token.Rbrace
           ; Token.Else
           ; Token.Lbrace
           ; Token.Return
           ; Token.False
           ; Token.Semicolon
           ; Token.Rbrace
           ; Token.Int 10
           ; Token.Eq
           ; Token.Int 10
           ; Token.Semicolon
           ; Token.Int 10
           ; Token.NotEq
           ; Token.Int 9
           ; Token.Semicolon
           ; Token.Eof
           ]
           {|
           let five = 5;
           let ten = 10;
           let add = fn(x,y) {
             x + y;
           };
           let result = add(five, ten);
           !-/*5;
           5 < 10 > 5;
           if (5 < 10) {
             return true;
           } else {
             return false;
           }
           10 == 10;
           10 != 9;
          |}
       ]
;;

let () = run_test_tt_main tests
