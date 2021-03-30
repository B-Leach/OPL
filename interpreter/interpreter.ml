exception Eval_error
exception Type_error

type exp =
  | True
  | False
  | If of exp * exp * exp
  | Num of int
  | IsZero of exp
  | Plus of exp * exp
  | Mult of exp * exp

type typ = TBool | TInt

let rec step (e : exp) = match e with
  | If(e1, e2, e3) -> (match e1 with
                          | True -> e2
                          | False -> e3
                          | Num(n1) -> raise Eval_error
                          | _ -> If(step e1, e2, e3))
  | Plus(e1, e2) -> (match e1 with
                        | True -> raise Eval_error
                        | False -> raise Eval_error
                        | Num(n1) -> (match e2 with
                                        | True -> raise Eval_error
                                        | False -> raise Eval_error
                                        | Num(n2) -> Num(n1 + n2)
                                        | _ -> Plus(e1, step e2))
                        | _ -> Plus(step e1, e2))
  | Mult(e1, e2) -> (match e1 with
                        | True -> raise Eval_error
                        | False -> raise Eval_error
                        | Num(n1) -> (match e2 with
                                        | True -> raise Eval_error
                                        | False -> raise Eval_error
                                        | Num(n2) -> Num(n1 * n2)
                                        | _ -> Mult(e1, step e2))
                        | _ -> Mult(step e1, e2))
  | IsZero(e1) -> (match e1 with
                      | True -> raise Eval_error
                      | False -> raise Eval_error
                      | Num(n1) -> if n1 = 0 then True else False
                      | _ -> IsZero(step e1))
  | _ -> raise Eval_error

let rec multi_step (e : exp) = match e with
  | True -> True
  | False -> False
  | Num(n1) -> Num(n1)
  | _ -> multi_step(step(e))

let rec type_check (e : exp) = match e with
  | True -> TBool
  | False -> TBool
  | Num(n) -> TInt
  | If(e1, e2, e3) -> if type_check e1 = TBool
                      then (if type_check e2 = type_check e3 then type_check e2 else raise Type_error)
                      else raise Type_error
  | IsZero(e1) -> if type_check e1 = TInt then TBool else raise Type_error
  | Plus(e1, e2) -> if type_check e1 = TInt then (if type_check e2 = TInt then TInt else raise Type_error) else raise Type_error
  | Mult(e1, e2) -> if type_check e1 = TInt then (if type_check e2 = TInt then TInt else raise Type_error) else raise Type_error

let rec string_of_exp (e : exp) = match e with
    | True -> "true"
    | False -> "false"
    | If(e1, e2, e3) -> "if " ^ string_of_exp e1 ^ " then " ^ string_of_exp e2
        ^ " else " ^ string_of_exp e3
    | Num(n1) -> string_of_int n1
    | IsZero(e1) -> "(isZero " ^ string_of_exp e1 ^ ")"
    | Plus(e1, e2) -> "(" ^ string_of_exp e1 ^ " + " ^ string_of_exp e2 ^ ")"
    | Mult(e1, e2) -> "(" ^ string_of_exp e1 ^ " * " ^ string_of_exp e2 ^ ")"


let () =
  print_endline( string_of_exp( (multi_step True) ));;
  print_endline( string_of_exp( (multi_step False) ));;
  print_endline( string_of_exp( (multi_step (Num 0)) ));;
  print_endline( string_of_exp( (multi_step (IsZero (Num 0))) ));;
  print_endline( string_of_exp( (multi_step (IsZero (Plus (Num 1, Num 1)))) ));;
  print_endline( string_of_exp( (multi_step (IsZero (Plus (Plus (Num 2, Num (-1)), Num 1)))) ));;
  print_endline( string_of_exp( (multi_step (Plus (Plus (Num (-1), Num 1), Plus (Num (-1), Num 1)))) ));;
  print_endline( string_of_exp( (multi_step (Plus (Num (-1), Plus (Mult (Num 2, Num 2), Num 1)))) ));;
  print_endline( string_of_exp( (multi_step (Plus (Plus (Plus (Num 2, Num (-1)), Num 1), Num (-1)))) ));;
  (* print_endline( string_of_exp( (multi_step (Plus (IsZero (Plus (Num (-1), Num 1)), Num 1))) ));; *)
  (* print_endline( string_of_exp( (multi_step (IsZero (If (IsZero (Num 0), True, Num 0)))) ));; *)
  (* print_endline( string_of_exp( (multi_step
                   (IsZero
                      (If
                         ( IsZero (Mult (Num 5, Num 0))
                         , If (False, Num 0, IsZero (Plus (Num (-1), Num 0)))
                         , Num 0 )))) ));; *)
  print_endline( string_of_exp( (multi_step (If (IsZero (Plus (Num (-1), Num 1)), Num 2, True))) ));;
  print_endline( string_of_exp( (multi_step
                   (If
                      ( If (IsZero (Mult (Plus (Num 1, Num (-1)), Num 1)), False, True)
                      , Mult (Num 1, Num 2)
                      , True ))) ));;
  print_endline( string_of_exp( (multi_step
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
  print_endline( string_of_exp( (multi_step
                   (If
                      ( True
                      , If (True, Mult (If (False, Num 0, Num 1), Num 1), Num 5)
                      , Plus (Mult (Num 4, Num 1), Num 1) ))) ));;
  print_endline( string_of_exp( (multi_step
                   (If
                      ( IsZero (If (IsZero (Plus (Num (-1), Num 2)), Num 0, Num 1))
                      , If
                          ( True
                          , If (False, Mult (Num 0, Num 6), Plus (Num 0, Num 1))
                          , Num 5 )
                      , Num 5 ))) ));;
  (* print_endline( string_of_exp( (multi_step
                   (If
                      ( IsZero (Plus (Num (-1), Plus (Num 1, Plus (Num (-1), Num 1))))
                      , IsZero True
                      , Num 1 ))) ));; *)
  print_endline( string_of_exp( (multi_step
                   (Plus
                      ( Num 1
                      , Plus
                          ( Num (-1)
                          , If
                              ( IsZero (Plus (Num 1, If (True, Num 1, Num 2)))
                              , Plus (Num 1, Num 2)
                              , Mult (Num 2, Num 2) ) ) ))) ));;
  (* print_endline( string_of_exp( (multi_step
                   (Plus
                      ( Num (-1)
                      , If
                          ( IsZero (Plus (Num 5, Num (-4)))
                          , Mult (Num 123, Plus (Num 5, Num (-4)))
                          , IsZero (Num 0) ) ))) ));; *)
