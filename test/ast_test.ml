open Base
open OUnit2
open Interpreter

let make_ast_test name expected input =
  name >:: fun _ -> assert_equal ~printer:Ast.show expected input

let tests =
  "ast tests"
  >::: [
         make_ast_test "parse let binding of integer"
           (Ast.Program
              {
                statements =
                  [
                    Ast.LetBinding
                      {
                        identifier = { name = "five" };
                        value = Ast.Integer { number = 104 };
                      };
                  ];
              })
           ({| let five = 104; |} |> Lexer.init |> Parser.init |> Parser.parse);
         make_ast_test "parse let binding of interger addition"
           (Ast.Program
              {
                statements =
                  [
                    Ast.LetBinding
                      {
                        identifier = { name = "five" };
                        value =
                          Ast.Operator
                            {
                              left = 100;
                              operator = Token.Plus;
                              right = Ast.Integer { number = 200 };
                            };
                      };
                  ];
              })
           ({| let five = 100 + 200; |} |> Lexer.init |> Parser.init
          |> Parser.parse);
         make_ast_test "parse let binding of interger addition in two steps"
           (Ast.Program
              {
                statements =
                  [
                    Ast.LetBinding
                      {
                        identifier = { name = "five" };
                        value =
                          Ast.Operator
                            {
                              left = 100;
                              operator = Token.Plus;
                              right =
                                Ast.Operator
                                  {
                                    left = 200;
                                    operator = Token.Plus;
                                    right = Ast.Integer { number = 300 };
                                  };
                            };
                      };
                  ];
              })
           ({|let five = 100 + 200 + 300;|} |> Lexer.init |> Parser.init
          |> Parser.parse);
         make_ast_test "parse let binding of interger addition in three steps"
           (Ast.Program
              {
                statements =
                  [
                    Ast.LetBinding
                      {
                        identifier = { name = "five" };
                        value =
                          Ast.Operator
                            {
                              left = 100;
                              operator = Token.Plus;
                              right =
                                Ast.Operator
                                  {
                                    left = 200;
                                    operator = Token.Plus;
                                    right =
                                      Ast.Operator
                                        {
                                          left = 300;
                                          operator = Token.Plus;
                                          right = Ast.Integer { number = 400 };
                                        };
                                  };
                            };
                      };
                  ];
              })
           ({|let five = 100 + 200 + 300 + 400;|} |> Lexer.init |> Parser.init
          |> Parser.parse);
       ]

let () = run_test_tt_main tests
