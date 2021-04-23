open List

exception Eval_error
exception Type_error
exception Substitution_error

type typ =
  | TBool
  | TInt
  | TArrow of typ * typ
  | TRef of typ
  | TUnit

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
  | LambdaRec of string * typ * typ * string * exp
  | Div of exp * exp
  | Try of exp * exp
  | RaiseDivByZero of typ * exp
  | Label of int
  | Malloc of exp
  | Mread of exp
  | Assign of exp * exp
  | Sequence of exp * exp
  | Unit

type type_environment = (string * typ) list
type memory = (int * exp) list

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
  | LambdaRec(s1, t1, t2, s2, e1) -> (match (free_variables e1) with
                                      | a :: s1 :: b :: s2 :: c -> a :: b :: c
                                      | _ -> raise Substitution_error)
  | Div(i1,i2) -> free_variables i1 @ free_variables i2
  | Try(i1, i2) -> free_variables i1 @ free_variables i2
  | RaiseDivByZero(typ, i1) -> free_variables i1
  | Label(n) -> []
  | Malloc(e1) -> free_variables e1
  | Mread(e1) -> free_variables e1
  | Assign(e1, e2) -> free_variables e1 @ free_variables e2
  | Sequence(e1, e2) -> free_variables e1 @ free_variables e2
  | Unit -> []

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
  | LambdaRec(s1, t1, t2, s2, i1) -> (if s1 = x
                                      then LambdaRec(s1, t1, t2, s2, i1)
                                      else (if s2 = x
                                            then LambdaRec(s1, t1, t2, s2, i1)
                                            else LambdaRec(s1, t1, t2, s2, substitution i1 x e2)))
  | Div(i1,i2) -> Div(substitution i1 x e2, substitution i2 x e2)
  | Try(i1, i2) -> Try(substitution i1 x e2, substitution i2 x e2)
  | RaiseDivByZero(typ, i1) -> RaiseDivByZero(typ, substitution i1 x e2)
  | Label(n) -> Label(n)
  | Malloc(i1) -> Malloc(substitution i1 x e2)
  | Mread(i1) -> Mread(substitution i1 x e2)
  | Assign(i1, i2) -> Assign(substitution i1 x e2, substitution i2 x e2)
  | Sequence(i1, i2) -> Sequence(substitution i1 x e2, substitution i2 x e2)
  | Unit -> Unit

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
  | Var(var) -> (match assoc_opt var te with
                  | None -> raise Type_error
                  | _ -> assoc var te)
  | Lambda(s1, t1, i1) -> TArrow(t1, type_check ((s1,t1)::te) i1)
  | Apply(i1 ,i2) -> (match type_check te i1 with
                      | TArrow(typ1, typ2) -> if typ1 = type_check te i2
                                              then typ2
                                              else raise Type_error
                      | _ -> raise Type_error)
  | LambdaRec(s1, t1, t2, s2, e1) -> TArrow(t1, t2)
  | Div(e1,e2) -> if type_check te e1 = TInt then (if type_check te e2 = TInt then TInt else raise Type_error) else raise Type_error
  | Try(e1, e2) -> (match type_check te e2 with
                    | TArrow(TInt, t) -> if type_check te e1 = t
                                  then t
                                  else raise Type_error
                    | _ -> raise Type_error )
  | RaiseDivByZero(typ, e1) -> typ

let rec step (e : exp) (m : memory) : (exp * memory ) = match e with
  | If (RaiseDivByZero(t, v), _, _) -> (RaiseDivByZero(t, v), m)
  | If(True, e1, _) -> (e1, m)
  | If(False, _, e1) -> (e1, m)
  | If(Num(n), _, _) -> raise Eval_error
  | If(e1, e2, e3) -> (match step e1 m with
                        | (ne, nm) -> If(ne , e2, e3), nm)
  | Plus(RaiseDivByZero(t, v), _) -> (RaiseDivByZero(t, v), m)
  | Plus(_, RaiseDivByZero(t, v)) -> (RaiseDivByZero(t, v), m)
  | Plus(e1, e2) -> (match e1 with
                        | True -> raise Eval_error
                        | False -> raise Eval_error
                        | Num(n1) -> (match e2 with
                                        | True -> raise Eval_error
                                        | False -> raise Eval_error
                                        | Num(n2) -> (Num(n1 + n2), m)
                                        | _ -> (match step e2 m with
                                                  | (ne, nm) -> Plus(e1, ne), nm ))
                        | _ -> (match step e1 m with
                                  | (ne, nm) -> Plus(ne, e2), nm))
  | Mult(RaiseDivByZero(t, v), _) -> (RaiseDivByZero(t, v), m)
  | Mult(_, RaiseDivByZero(t, v)) -> (RaiseDivByZero(t, v), m)
  | Mult(e1, e2) -> (match e1 with
                        | True -> raise Eval_error
                        | False -> raise Eval_error
                        | Num(n1) -> (match e2 with
                                        | True -> raise Eval_error
                                        | False -> raise Eval_error
                                        | Num(n2) -> (Num(n1 * n2), m)
                                        | _ -> (match step e2 m with
                                                  | (ne, nm) -> Mult(e1, ne), nm ))
                        | _ -> (match step e1 m with
                                  | (ne, nm) -> Mult(ne, e2), nm))
  | IsZero(RaiseDivByZero(t, v)) -> (RaiseDivByZero(t, v), m)
  | IsZero(e1) -> (match e1 with
                      | True -> raise Eval_error
                      | False -> raise Eval_error
                      | Num(n1) -> if n1 = 0 then (True, m) else (False, m)
                      | _ -> (match step e1 m with
                                | (ne, nm) -> IsZero(ne), nm ))
  | Apply( RaiseDivByZero(t, v), _) -> (RaiseDivByZero(t, v), m)
  | Apply( _, RaiseDivByZero(t, v)) -> (RaiseDivByZero(t, v), m)
  | Apply( Lambda(var, typ, body), True ) -> (substitution body var True, m)
  | Apply( Lambda(var, typ, body), False ) -> (substitution body var False, m)
  | Apply( Lambda(var, typ, body), (Num(n1)) ) -> (substitution body var (Num(n1)), m)
  | Apply( Lambda(var1, typ1, body1), (Lambda(var2, typ2, body2)) ) -> (substitution body1 var1 (Lambda(var2, typ2, body2)), m)
  | Apply( Lambda(var, typ, body), arg ) -> (match step arg m with
                                              | (ne, nm) -> step (Apply(Lambda(var, typ, body), ne)) nm)
  | Apply( LambdaRec(f, typ1, typ2, var, body), arg) -> (substitution (substitution body var arg) f (LambdaRec(f, typ1, typ2, var, body)), m)
  | Apply( func, arg ) -> (match step func m with
                            | (ne, nm) -> step (Apply(ne, arg)) nm)
  | Div( e1, Num(0)) -> (RaiseDivByZero(TInt, e1), m)
  | Div( Num(n1), Num(n2)) -> (Num(n1 / n2), m)
  | Div( e1, e2) -> (match step e1 m with
                      | (ne1, nm1) -> ( match step e2 nm1 with
                                        | (ne2, nm2) -> (Div(ne1, ne2), nm2) ))
  | Try(True, _) -> (True, m)
  | Try(False, _) -> (False, m)
  | Try(Num(n1), _) -> (Num(n1), m)
  | Try(RaiseDivByZero(t, v), e1) -> (Apply(e1, v), m)
  | Try(v, e1) -> (match step v m with
                    | (ne, nm) -> (Try(ne, e1), nm))
  | Malloc(Malloc(v)) -> (match step (Malloc(v)) m with
                            | (ne, nm) -> (Malloc(ne), nm))
  | Malloc(v) -> (Label(List.length m), ((List.length m, v)::m))
  | Mread(Label(n)) -> (match assoc_opt n m with
                        | None -> raise Eval_error
                        | _ -> ((assoc n m), m))
  | Mread(e1) -> (match step e1 m with
                    | (ne, nm) -> (Mread(ne), nm))
  | _ -> raise Eval_error

let rec multi_step (e : exp) (m : memory) : (exp * memory ) = match e with
  | True -> (True, m)
  | False -> (False, m)
  | Num(n1) -> (Num(n1), m)
  | Var(var) -> (Var(var), m)
  | Label(n) -> (Label(n), m)
  | Lambda(var, typ, body) -> (Lambda(var, typ, body), m)
  | LambdaRec(s1, t1, t2, s2, e1) -> (LambdaRec(s1, t1, t2, s2, e1), m)
  | RaiseDivByZero(t, True) -> (RaiseDivByZero(t, True), m)
  | RaiseDivByZero(t, False) -> (RaiseDivByZero(t, False), m)
  | RaiseDivByZero(t, Num(n)) -> (RaiseDivByZero(t, Num(n)), m)
  | RaiseDivByZero(t, RaiseDivByZero(t2, v)) -> (RaiseDivByZero(t, v), m)
  | RaiseDivByZero(t, e1) -> (match step e1 m with
                                | (ne, nm) -> (RaiseDivByZero(t, ne), nm))
  | _ -> (match step e m with
            | (ne, nm) -> multi_step ne nm)
