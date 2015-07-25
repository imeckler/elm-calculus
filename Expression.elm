module Expression
  ( Expression(..)
  , Environment
  , evaluate
  , evaluateExn
  , parse
  ) where

import Dict
import Maybe
import Native.ParseExpression

type Expression
  = Var String
  | Constant Float
  | Mul Expression Expression
  | Add Expression Expression
  | LogBase Float Expression
  | Exp Float Expression
  | Pow Expression Float

type alias Environment = Dict.Dict String Float

parse : String -> Result String Expression
parse = Native.ParseExpression.parse

maybeMap2 : (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
maybeMap2 f ma mb =
  case ma of
    Just a ->
      case mb of
        Just b -> Just (f a b)
        Nothing -> Nothing
    Nothing -> Nothing

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

    LogBase b e1 ->
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

    LogBase b e1 ->
      logBase b (evaluate env e1)

    Exp b e1 ->
      b ^ (evaluate env e1)

    Pow e1 p ->
      (evaluate env e1) ^ p

