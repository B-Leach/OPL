exception Eval_error

type exp =
  | True
  | False
  | If of exp * exp * exp
  | Num of int
  | IsZero of exp
  | Plus of exp * exp
  | Mult of exp * exp


let rec string_of_exp (e : exp) = match e with
  | True -> "true"
  | False -> "false"
  | If(e1, e2, e3) -> "if " ^ string_of_exp e1 ^ " then " ^ string_of_exp e2
      ^ " else " ^ string_of_exp e3
  | Num(n1) -> string_of_int n1
  | IsZero(e1) -> "(isZero " ^ string_of_exp e1 ^ ")"
  | Plus(e1, e2) -> "(" ^ string_of_exp e1 ^ " + " ^ string_of_exp e2 ^ ")"
  | Mult(e1, e2) -> "(" ^ string_of_exp e1 ^ " * " ^ string_of_exp e2 ^ ")"

let rec eval (e : exp) = match e with
  | True -> True
  | False -> False
  | If(e1, e2, e3) -> (match eval(e1) with
                        | True -> eval(e2)
                        | False -> eval(e3)
                        | _ -> raise Eval_error)
  | Num(n1) -> Num(n1)
  | IsZero(e1) -> (match eval(e1) with
                    | Num(n1) -> if n1 = 0 then True else False
                    | _ -> raise Eval_error)
  | Plus(e1, e2) -> (match eval(e1) with
                      | Num(n1) -> (match eval(e2) with
                                      | Num(n2) -> Num( n1 + n2 )
                                      | _ -> raise Eval_error)
                      | _ -> raise Eval_error)
  | Mult(e1, e2) -> (match eval(e1) with
                      | Num(n1) -> (match eval(e2) with
                                      | Num(n2) -> Num( n1 * n2 )
                                      | _ -> raise Eval_error)
                      | _ -> raise Eval_error)


let () =
  print_endline( string_of_exp( (eval True) ));;
  print_endline( string_of_exp( (eval False) ));;
  print_endline( string_of_exp( (eval (Num 0)) ));;
  print_endline( string_of_exp( (eval (IsZero (Num 0))) ));;
  print_endline( string_of_exp( (eval (IsZero (Plus (Num 1, Num 1)))) ));;
  print_endline( string_of_exp( (eval (IsZero (Plus (Plus (Num 2, Num (-1)), Num 1)))) ));;
  print_endline( string_of_exp( (eval (Plus (Plus (Num (-1), Num 1), Plus (Num (-1), Num 1)))) ));;
  print_endline( string_of_exp( (eval (Plus (Num (-1), Plus (Mult (Num 2, Num 2), Num 1)))) ));;
  print_endline( string_of_exp( (eval (Plus (Plus (Plus (Num 2, Num (-1)), Num 1), Num (-1)))) ));;
  (* print_endline( string_of_exp( (eval (Plus (IsZero (Plus (Num (-1), Num 1)), Num 1))) ));; *)
  (* print_endline( string_of_exp( (eval (IsZero (If (IsZero (Num 0), True, Num 0)))) ));; *)
  (* print_endline( string_of_exp( (eval
                   (IsZero
                      (If
                         ( IsZero (Mult (Num 5, Num 0))
                         , If (False, Num 0, IsZero (Plus (Num (-1), Num 0)))
                         , Num 0 )))) ));; *)
  print_endline( string_of_exp( (eval (If (IsZero (Plus (Num (-1), Num 1)), Num 2, True))) ));;
  print_endline( string_of_exp( (eval
                   (If
                      ( If (IsZero (Mult (Plus (Num 1, Num (-1)), Num 1)), False, True)
                      , Mult (Num 1, Num 2)
                      , True ))) ));;
  print_endline( string_of_exp( (eval
                   (If
                      ( If (IsZero (Mult (Num 0, Num 0)), IsZero (Num 2), Num 0)
                      , Mult (Num 2, Mult (Num 1, Num 1))
                      , Plus
                          ( Plus
                              ( Plus
                                  ( Plus (If (IsZero (Num 0), Num 1, Num 0), Num (-1))
                                  , Num 1 )
                              , Num (-1) )
                          , Num 1 ) ))) ));;
  print_endline( string_of_exp( (eval
                   (If
                      ( True
                      , If (True, Mult (If (False, Num 0, Num 1), Num 1), Num 5)
                      , Plus (Mult (Num 4, Num 1), Num 1) ))) ));;
  print_endline( string_of_exp( (eval
                   (If
                      ( IsZero (If (IsZero (Plus (Num (-1), Num 2)), Num 0, Num 1))
                      , If
                          ( True
                          , If (False, Mult (Num 0, Num 6), Plus (Num 0, Num 1))
                          , Num 5 )
                      , Num 5 ))) ));;
  (* print_endline( string_of_exp( (eval
                   (If
                      ( IsZero (Plus (Num (-1), Plus (Num 1, Plus (Num (-1), Num 1))))
                      , IsZero True
                      , Num 1 ))) ));; *)
  print_endline( string_of_exp( (eval
                   (Plus
                      ( Num 1
                      , Plus
                          ( Num (-1)
                          , If
                              ( IsZero (Plus (Num 1, If (True, Num 1, Num 2)))
                              , Plus (Num 1, Num 2)
                              , Mult (Num 2, Num 2) ) ) ))) ));;
  (* print_endline( string_of_exp( (eval
                   (Plus
                      ( Num (-1)
                      , If
                          ( IsZero (Plus (Num 5, Num (-4)))
                          , Mult (Num 123, Plus (Num 5, Num (-4)))
                          , IsZero (Num 0) ) ))) ));; *)

let () =
  print_endline("");;
  print_endline( string_of_exp (Num(3)));;
  print_endline( string_of_exp (True));;
  print_endline( string_of_exp (False));;
  print_endline( string_of_exp (Plus(Num(3), Num(2))));;
  print_endline( string_of_exp (Mult(Num(3), Num(2))));;
  print_endline( string_of_exp (Plus(Num(3), Plus(Num(3), Mult(Num(2), Plus(Num(3), Num(2)))))));;
  print_endline( string_of_exp (If(True, Num(3), Num(5))));;
  print_endline( string_of_exp (If(False, Plus(Num(3), Num(2)), Plus(Num(5), Num(1)))));;
  print_endline( string_of_exp ( If( Plus(False, True), Plus(Num(3), False), Mult(Num(3), Num(1)) ) ) );;
  print_endline( string_of_exp ( If( IsZero(Num(1)), Plus(Num(3), Num(2)), Plus(Num(5), Num(1)) ) ) );;
  print_endline( string_of_exp ( IsZero(Mult(Num(3),Num(5))) ));;
  print_endline( string_of_exp ( IsZero( If(IsZero(Num(1)), Plus(Num(3), Num(2)), Plus(Num(5), Num(1)) ) ) ));;
  print_endline( string_of_exp ( Plus(Num(3), If(IsZero(Num(1)), Plus(Num(3), Num(2)), Plus(Num(5), Num(1)) )) ));;
  print_endline( string_of_exp ( Plus(Num(3), Mult(If(IsZero(Num(1)), Plus(Num(3), Num(2)), Plus(Num(5), Num(1))), IsZero(True)) ) ));;
  print_endline( string_of_exp ( If( If( True, True, False), Plus(Num(3),Num(2)), Plus(Num(5),Num(1))) ));;
  print_endline( string_of_exp ( If(True, If( IsZero(Mult(Num(3),Num(5))), Plus(Num(3),Num(2)), Plus(Num(5),Num(1))),
                                  If( True, Mult(Num(3),Num(2)), Mult(Num(2), Plus(Num(3), Num(2))) )) ));;
