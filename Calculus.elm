module Calculus where

import Expression exposing (Expression(..))

derivative : Expression -> String -> Expression
derivative expr x =
  case expr of
    Var y ->
      if y == x then Constant 1 else Constant 0

    Constant _ -> Constant 0

    Mul e1 e2 ->
      Add (Mul e1 (derivative e2)) (Mul (derivative e1) e2)

    Add e1 e2 ->
      Add (derivative e1) (derivative e2)

    LogBase b e1 ->
      let d = derivative e1 in
      Mul d (Pow (Mul (Constant (logBase e b)) e1) (-1))

    Exp b e1 ->
      let d = derivative e1 in
      Mul (Constant (logBase e b)) (Mul d expr)

    Pow e1 p ->
      Mul (derivative e1)
        (Mul (Constant p) (Pow e1 (p - 1)))

