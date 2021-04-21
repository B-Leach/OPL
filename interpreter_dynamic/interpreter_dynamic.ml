open List

exception Eval_error

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
  | Lambda of string * exp
  | Apply of exp * exp
  | Let of string * exp * exp
  | TypeError

type environment = (string * exp) list

let rec step (env : environment) (e : exp) : (environment * exp) = match e with
  | If(TypeError, _, _) -> (env, TypeError)
  | If(Var(var), e2, e3) -> (match assoc_opt var env with
                            | None -> (env, TypeError)
                            | _ -> step env (If((assoc var env), e2, e3)) )
  | If(e1, e2, e3) -> (match e1 with
                        | True -> (env, e2)
                        | False -> (env, e3)
                        | Num(n1) -> (env, TypeError)
                        | _ -> (match step env e1 with
                                  | (nenv, ne) -> step nenv (If(ne, e2, e3)) ))
  | Plus(TypeError, _) -> (env, TypeError)
  | Plus(_, TypeError) -> (env, TypeError)
  | Plus(Var(var), e2) -> (match assoc_opt var env with
                            | None -> (match step env e2 with
                                        | (nenv, ne) -> step nenv (Plus(Var(var), ne)))
                            | _ -> step env (Plus((assoc var env), e2)))
  | Plus(e1, Var(var)) -> step env (Plus(Var(var), e1))
  | Plus(e1, e2) -> (match e1 with
                        | True -> (env, TypeError)
                        | False -> (env, TypeError)
                        | Num(n1) -> (match e2 with
                                        | True -> (env, TypeError)
                                        | False -> (env, TypeError)
                                        | Num(n2) -> (env, Num(n1 + n2))
                                        | _ -> (match step env e2 with
                                                  | (nenv, ne) -> (nenv, Plus(e1, ne)) ) )
                        | _ -> (match step env e1 with
                                  | (nenv, ne) -> (nenv, Plus(ne, e2)) ) )
  | Mult(TypeError, _) -> (env, TypeError)
  | Mult(_, TypeError) -> (env, TypeError)
  | Mult(Var(var), e2) -> (match assoc_opt var env with
                            | None -> (match step env e2 with
                                        | (nenv, ne)-> step nenv (Mult(Var(var), ne)))
                            | _ -> step env (Mult((assoc var env), e2)))
  | Mult(e1, Var(var)) -> step env (Mult(Var(var), e1))
  | Mult(e1, e2) -> (match e1 with
                        | True -> (env, TypeError)
                        | False -> (env, TypeError)
                        | Num(n1) -> (match e2 with
                                        | True -> (env, TypeError)
                                        | False -> (env, TypeError)
                                        | Num(n2) -> (env, Num(n1 * n2))
                                        | _ -> (match step env e2 with
                                                  | (nenv, ne) -> (nenv, Mult(e1, ne)) ) )
                        | _ -> (match step env e1 with
                                  | (nenv, ne) -> (nenv, Mult(ne, e2)) ) )
  | IsZero(TypeError) -> (env, TypeError)
  | IsZero(Var(var)) -> (match assoc_opt var env with
                            | None -> (env, TypeError)
                            | _ -> step env (IsZero(assoc var env)) )
  | IsZero(e1) -> (match e1 with
                      | True -> (env, TypeError)
                      | False -> (env, TypeError)
                      | Num(n1) -> if n1 = 0 then (env, True) else (env, False)
                      | _ -> (match step env e1 with
                                | (nenv, ne) -> (nenv, IsZero(ne)) ) )
  | Apply(TypeError, _) -> (env, TypeError)
  | Apply(_, TypeError) -> (env, TypeError)
  | Apply( Lambda(var, body), True ) -> (((var,True)::env), body)
  | Apply( Lambda(var, body), False ) -> (((var,False)::env), body)
  | Apply( Lambda(var, body), (Num(n1)) ) -> (((var,Num(n1))::env), body)
  | Apply( Lambda(var1, body), (Var(var2)) ) -> (match assoc_opt var2 env with
                                                  | None -> (((var1,Var(var2))::env), body)
                                                  | _ -> (((var1,(assoc var2 env))::env), body) )
  | Apply( Lambda(var1, body1), (Lambda(var2, body2)) ) ->  (((var1,(Lambda(var2, body2)))::env), body1)
  | Apply( Lambda(var, body), arg ) -> (match step env arg with
                                          | (nenv, ne) -> step nenv (Apply(Lambda(var, body), ne)))
  | Apply( Var(var), arg ) -> (match assoc_opt var env with
                                | None -> (env, TypeError)
                                | _ -> step env (Apply((assoc var env), arg)) )
  | Apply( func, arg ) -> (match step env func with
                    | (nenv, ne) -> step nenv (Apply(ne, arg)))
  | Let(var, e1, e2) -> step env (Apply(Lambda(var, e2), e1))
  | _ -> (env, TypeError)

let rec multi_step (env : environment) (e : exp) : (environment * exp) = match e with
  | True -> (env, True)
  | False -> (env, False)
  | Num(n1) -> (env, Num(n1))
  | Var(var) -> (match assoc_opt var env with
                  | None -> (env, Var(var))
                  | _ -> multi_step env (assoc var env))
  | Lambda(var, body) -> (env, Lambda(var, body))
  | TypeError -> (env, TypeError)
  | _ -> (match step env e with
          | (nenv, ne) -> multi_step nenv ne)
