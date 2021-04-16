exception Eval_error
exception Type_error
exception Substitution_error

type typ =
  | TBool
  | TInt
  | TArrow of typ * typ

type exp =
  | True
  | False
  | If of exp * exp * exp
  | Num of int
  | IsZero of exp
  | Plus of exp * exp
  | Mult of exp * exp
  | Var of string
  | Lambda of string * typ * exp
  | Apply of exp * exp

type type_environment = (string * typ) list

(* >/ *)
let rec free_variables (e : exp) = match e with
  | True -> []
  | False -> []
  | If(i1, i2, i3) -> free_variables i1 @ free_variables i2 @ free_variables i3
  | Num(n) -> []
  | IsZero(i1) -> free_variables i1
  | Plus(i1, i2) -> free_variables i1 @ free_variables i2
  | Mult(i1, i2) -> free_variables i1 @ free_variables i2
  | Var(var) -> var :: []
  | Lambda(s1, t1, i1) -> (match (free_variables i1) with
                          | a :: s1 :: b -> a :: b
                          | _ -> raise Substitution_error)
  | Apply(i1, i2) -> free_variables i1 @ free_variables i2

(* >/ *)
let rec substitution (e1 : exp) (x : string) (e2 : exp) = match e1 with
  | True -> True
  | False -> False
  | If (i1, i2, i3) -> If (substitution i1 x e2, substitution i2 x e2, substitution i3 x e2 )
  | Num(n1) -> Num(n1)
  | IsZero(i1) -> IsZero(substitution i1 x e2)
  | Plus(i1, i2) -> Plus (substitution i1 x e2, substitution i2 x e2)
  | Mult(i1, i2) -> Mult (substitution i1 x e2, substitution i2 x e2)
  | Var(var) -> if var = x then e2 else Var(var)
  | Lambda(s1, t1, i1) -> if s1 = x then Lambda(s1, t1, i1) else Lambda(s1, t1, substitution i1 x e2)
  | Apply(i1, i2) -> Apply (substitution i1 x e2, substitution i2 x e2)

(* Add Var, Lambda, Apply *)
let rec type_check (te : type_environment) (e : exp) = match e with
  | True -> TBool
  | False -> TBool
  | Num(n) -> TInt
  | If(e1, e2, e3) -> if type_check te e1 = TBool
                      then (if type_check te e2 = type_check te e3 then type_check te e2 else raise Type_error)
                      else raise Type_error
  | IsZero(e1) -> if type_check te e1 = TInt then TBool else raise Type_error
  | Plus(e1, e2) -> if type_check te e1 = TInt then (if type_check te e2 = TInt then TInt else raise Type_error) else raise Type_error
  | Mult(e1, e2) -> if type_check te e1 = TInt then (if type_check te e2 = TInt then TInt else raise Type_error) else raise Type_error
  | Var(var) -> (match te with
                  | a :: (var, typ) :: t -> typ
                  | _ -> raise Eval_error)
  | Lambda(s1, t1, i1) -> TArrow(t1, type_check ((s1,t1)::te) i1)
  | Apply(i1 ,i2) -> match type_check te i1 with
                      | TArrow(typ1, typ2) -> if typ2 = type_check te i2
                                              then typ2
                                              else raise Eval_error

(* Add Apply >/ *)
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
  | Apply( Lambda(var, typ, body), True ) -> substitution body var True
  | Apply( Lambda(var, typ, body), False ) -> substitution body var False
  | Apply( Lambda(var, typ, body), (Num(n1)) ) -> substitution body var (Num(n1))
  | Apply( Lambda(var1, typ1, body1), (Lambda(var2, typ2, body2)) ) -> substitution body1 var1 (Lambda(var2, typ2, body2))
  | Apply( Lambda(var, typ, body), arg ) -> step(Apply( Lambda(var, typ, body), step(arg) ))
  | Apply( func, arg ) -> step( Apply( step(func), arg) )
  | _ -> raise Eval_error

(* >/ *)
let rec multi_step (e : exp) = match e with
  | True -> True
  | False -> False
  | Num(n1) -> Num(n1)
  | Var(var) -> Var(var)
  | Lambda(var, typ, body) -> Lambda(var, typ, body)
  | _ -> multi_step(step(e))



(*let rec string_of_exp (e : exp) = match e with
    | True -> "true"
    | False -> "false"
    | If(e1, e2, e3) -> "if " ^ string_of_exp e1 ^ " then " ^ string_of_exp e2
        ^ " else " ^ string_of_exp e3
    | Num(n1) -> string_of_int n1
    | IsZero(e1) -> "(isZero " ^ string_of_exp e1 ^ ")"
    | Plus(e1, e2) -> "(" ^ string_of_exp e1 ^ " + " ^ string_of_exp e2 ^ ")"
    | Mult(e1, e2) -> "(" ^ string_of_exp e1 ^ " * " ^ string_of_exp e2 ^ ")"*)
(*let () =
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
                          , IsZero (Num 0) ) ))) ));; *)*)
