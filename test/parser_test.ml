open Base
open OUnit2
open Interpreter

let make_ast_test name expected input =
  name >:: fun _ -> assert_equal ~printer:Ast.show expected input

let make_int_test name expected input =
  name >:: fun _ -> assert_equal ~printer:Int.to_string expected input

let input = {|
  let five = 5;
  let ten = 10;
  let hundred = 100;
  |}

let tests =
  "parser tests"
  >::: [
         (* TODO: make green
            make_ast_test ""
              (Ast.Program { statements = [] })
              ({| *)
               5 + 5;
               5 - 5;
               5 * 5;
               5 / 5;
               5 > 5;
               5 < 5;
               5 == 5;
               5 != 5;
               |}
             |> Parser.parse); *)
         make_ast_test "parse prefix operators"
           (Ast.Program
              {
                statements =
                  [
                    Ast.ExpressionStatement
                      {
                        value =
                          Ast.Prefix
                            {
                              prefix = Token.Bang;
                              precedence = 6;
                              expression = Ast.Integer { number = 5 };
                            };
                      };
                    Ast.ExpressionStatement
                      {
                        value =
                          Ast.Prefix
                            {
                              prefix = Token.Minus;
                              precedence = 6;
                              expression = Ast.Integer { number = 15 };
                            };
                      };
                  ];
              })
           ({| 
              !5;
              -15;
           |}
          |> Parser.parse);
         make_ast_test "parse integer literal"
           (Ast.Program
              {
                statements =
                  [
                    Ast.ExpressionStatement
                      { value = Ast.Integer { number = 5 } };
                  ];
              })
           ({| 5; |} |> Parser.parse);
         make_ast_test "parse identifier expression"
           (Ast.Program
              {
                statements =
                  [
                    Ast.ExpressionStatement
                      { value = Ast.Identifier { name = "foobar" } };
                  ];
              })
           ({| foobar |} |> Parser.parse);
         make_ast_test "parse return statements"
           (Ast.Program
              {
                statements =
                  [
                    Ast.ReturnStatement { value = Ast.Integer { number = 10 } };
                    Ast.ReturnStatement { value = Ast.Integer { number = 10 } };
                    Ast.ReturnStatement { value = Ast.Integer { number = 10 } };
                  ];
              })
           ({|  return 5; 
                return 10; 
                return 100; |}
          |> Parser.parse);
         make_int_test "parse program of 3 return statements" 3
           (input |> Parser.parse |> function
            | Program p -> List.length p.statements
            | _ -> 0);
         make_ast_test "parse let statements"
           (Ast.Program
              {
                statements =
                  [
                    Ast.LetStatement
                      {
                        identifier = { name = "five" };
                        value = Ast.Integer { number = 10 };
                      };
                    Ast.LetStatement
                      {
                        identifier = { name = "ten" };
                        value = Ast.Integer { number = 10 };
                      };
                    Ast.LetStatement
                      {
                        identifier = { name = "hundred" };
                        value = Ast.Integer { number = 10 };
                      };
                  ];
              })
           ({|  let five = 5; 
                let ten = 10; 
                let hundred = 100; |}
          |> Parser.parse);
         make_int_test "parse program of 3 let statements" 3
           (input |> Parser.parse |> function
            | Program p -> List.length p.statements
            | _ -> 0);
       ]

let () = run_test_tt_main tests
