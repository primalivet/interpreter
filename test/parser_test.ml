open Base
open OUnit2
open Interpreter

let make_ast_test name expected input =
  name >:: fun _ -> assert_equal ~printer:Ast.show expected input
;;

let make_int_test name expected input =
  name >:: fun _ -> assert_equal ~printer:Int.to_string expected input
;;

let make_ast_test_str name expected input =
  name
  >:: fun _ -> assert_equal ~printer:String.to_string expected (Ast.to_literal_str input)
;;

let make_ast_test_str_list name (pairs : (string * string) list) =
  name
  >:: fun _ ->
  pairs
  |> List.iter ~f:(fun (expected, input) ->
       assert_equal
         ~printer:String.to_string
         expected
         (input |> Parser.parse |> Ast.to_literal_str);
       ())
;;

let input = {|
  let five = 5;
  let ten = 10;
  let hundred = 100;
  |}

let tests =
  "parser tests"
  >::: [ make_ast_test
           "boolean"
           (Ast.Program
              { statements =
                  [ Ast.ExpressionStatement { value = Ast.Boolean { value = true } }
                  ; Ast.ExpressionStatement { value = Ast.Boolean { value = false } }
                  ; Ast.LetStatement
                      { identifier = { name = "t" }
                      ; value = Ast.Boolean { value = true }
                      }
                  ; Ast.LetStatement
                      { identifier = { name = "f" }
                      ; value = Ast.Boolean { value = false }
                      }
                  ]
              })
           ({| true 
               false 
               let t = true;
               let f = false; |}
            |> Parser.parse)
       ; make_ast_test_str_list
           "parse expression statements"
           [ "((-a) * b)", "-a * b"
           ; "(!(-a))", "!-a"
           ; "((a + b) + c)", "a + b + c"
           ; "((a + b) - c)", "a + b - c"
           ; "((a * b) * c)", "a * b * c"
           ; "((a * b) / c)", "a * b / c"
           ; "(a + (b / c))", "a + b / c"
           ; "(((a + (b * c)) + (d / e)) - f)", "a + b * c + d / e - f"
           ; "(3 + 4)((-5) * 5)", "3 + 4; -5 * 5"
           ; "((5 > 4) == (3 < 4))", "5 > 4 == 3 < 4"
           ; "((5 < 4) != (3 > 4))", "5 < 4 != 3 > 4"
           ; "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))", "3 + 4 * 5 == 3 * 1 + 4 * 5"
           ]
       ; make_ast_test_str "ast as string" "(2 + (5 * 3))" ("2 + 5 * 3;" |> Parser.parse)
       ; make_ast_test
           ""
           (Ast.Program
              { statements =
                  [ Ast.ExpressionStatement
                      { value =
                          Ast.Infix
                            { left = Ast.Integer { number = 5 }
                            ; operator = Token.Plus
                            ; right = Ast.Integer { number = 5 }
                            }
                      }
                  ; Ast.ExpressionStatement
                      { value =
                          Ast.Infix
                            { left = Ast.Integer { number = 5 }
                            ; operator = Token.Minus
                            ; right = Ast.Integer { number = 5 }
                            }
                      }
                  ; Ast.ExpressionStatement
                      { value =
                          Ast.Infix
                            { left = Ast.Integer { number = 5 }
                            ; operator = Token.Asterisk
                            ; right = Ast.Integer { number = 5 }
                            }
                      }
                  ; Ast.ExpressionStatement
                      { value =
                          Ast.Infix
                            { left = Ast.Integer { number = 5 }
                            ; operator = Token.Slash
                            ; right = Ast.Integer { number = 5 }
                            }
                      }
                  ; Ast.ExpressionStatement
                      { value =
                          Ast.Infix
                            { left = Ast.Integer { number = 5 }
                            ; operator = Token.Gt
                            ; right = Ast.Integer { number = 5 }
                            }
                      }
                  ; Ast.ExpressionStatement
                      { value =
                          Ast.Infix
                            { left = Ast.Integer { number = 5 }
                            ; operator = Token.Lt
                            ; right = Ast.Integer { number = 5 }
                            }
                      }
                  ; Ast.ExpressionStatement
                      { value =
                          Ast.Infix
                            { left = Ast.Integer { number = 5 }
                            ; operator = Token.Eq
                            ; right = Ast.Integer { number = 5 }
                            }
                      }
                  ; Ast.ExpressionStatement
                      { value =
                          Ast.Infix
                            { left = Ast.Integer { number = 5 }
                            ; operator = Token.NotEq
                            ; right = Ast.Integer { number = 5 }
                            }
                      }
                  ]
              })
           ({| 
               5 + 5;
               5 - 5;
               5 * 5;
               5 / 5;
               5 > 5;
               5 < 5;
               5 == 5;
               5 != 5;
               |}
            |> Parser.parse)
       ; make_ast_test
           "parse prefix operators"
           (Ast.Program
              { statements =
                  [ Ast.ExpressionStatement
                      { value =
                          Ast.Prefix
                            { prefix = Token.Bang
                            ; expression = Ast.Integer { number = 5 }
                            }
                      }
                  ; Ast.ExpressionStatement
                      { value =
                          Ast.Prefix
                            { prefix = Token.Minus
                            ; expression = Ast.Integer { number = 15 }
                            }
                      }
                  ]
              })
           ({| 
              !5;
              -15;
           |} |> Parser.parse)
       ; make_ast_test
           "parse integer literal"
           (Ast.Program
              { statements =
                  [ Ast.ExpressionStatement { value = Ast.Integer { number = 5 } } ]
              })
           ({| 5; |} |> Parser.parse)
       ; make_ast_test
           "parse identifier expression"
           (Ast.Program
              { statements =
                  [ Ast.ExpressionStatement { value = Ast.Identifier { name = "foobar" } }
                  ]
              })
           ({| foobar |} |> Parser.parse)
       ; make_ast_test
           "parse return statements"
           (Ast.Program
              { statements =
                  [ Ast.ReturnStatement { value = Ast.Integer { number = 10 } }
                  ; Ast.ReturnStatement { value = Ast.Integer { number = 10 } }
                  ; Ast.ReturnStatement { value = Ast.Integer { number = 10 } }
                  ]
              })
           ({|  return 5; 
                return 10; 
                return 100; |}
            |> Parser.parse)
       ; make_int_test
           "parse program of 3 return statements"
           3
           (input
            |> Parser.parse
            |> function
            | Program p -> List.length p.statements
            | _ -> 0)
       ; make_ast_test
           "parse let statements"
           (Ast.Program
              { statements =
                  [ Ast.LetStatement
                      { identifier = { name = "five" }
                      ; value = Ast.Integer { number = 5 }
                      }
                  ; Ast.LetStatement
                      { identifier = { name = "ten" }
                      ; value = Ast.Integer { number = 10 }
                      }
                  ; Ast.LetStatement
                      { identifier = { name = "hundred" }
                      ; value = Ast.Integer { number = 100 }
                      }
                  ]
              })
           ({|  let five = 5; 
                let ten = 10; 
                let hundred = 100; |}
            |> Parser.parse)
       ; make_int_test
           "parse program of 3 let statements"
           3
           (input
            |> Parser.parse
            |> function
            | Program p -> List.length p.statements
            | _ -> 0)
       ]
;;

let () = run_test_tt_main tests
