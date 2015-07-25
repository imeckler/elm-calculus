module Expression
  ( Expression(..)
  , Environment
  , evaluate
  , evaluateExn
  ) where

import Dict
import Maybe
import Expression.Type

type alias Environment = Map String Float

maybeMap2 : (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c

evaluate : Expression -> (Environment -> Maybe Float)
evaluate expr env =
  case expr of
    Var x ->
      Dict.get x env

    Constant c ->
      Just c

    Mul e1 e2 ->
      maybeMap2 (*) (evaluate e1 env) (evaluate e2 env)

    Add e1 e2 ->
      maybeMap2 (+) (evaluate e1 env) (evaluate e2 env)

    Log b e1 ->
      Maybe.map (logBase b) (evaluate env e1)

    Exp b e1 ->
      Maybe.map (\x -> b ^ x) (evaluate env e1)

    Pow e1 p ->
      Maybe.map (\x -> x ^ p) (evaluate env e1)

evaluateExn : Expression -> (Environment -> Float)
evaluateExn expr env =
  case expr of
    Var x ->
      case Dict.get x env of { Just a -> a }

    Constant c ->
      c

    Mul e1 e2 ->
      evaluate e1 env * evaluate e2 env

    Add e1 e2 ->
      evaluate e1 env + evaluate e2 env

    Log b e1 ->
      logBase b (evaluate env e1)

    Exp b e1 ->
      b ^ (evaluate env e1)

    Pow e1 p ->
      (evaluate env e1) ^ p

