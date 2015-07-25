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
      Maybe.map (logBase b) (evaluate e1 env)

    Exp b e1 ->
      Maybe.map (\x -> b ^ x) (evaluate e1 env)

    Pow e1 p ->
      Maybe.map (\x -> x ^ p) (evaluate e1 env)

evaluateExn : Expression -> (Environment -> Float)
evaluateExn expr env =
  case expr of
    Var x ->
      case Dict.get x env of { Just a -> a }

    Constant c ->
      c

    Mul e1 e2 ->
      evaluateExn e1 env * evaluateExn e2 env

    Add e1 e2 ->
      evaluateExn e1 env + evaluateExn e2 env

    LogBase b e1 ->
      logBase b (evaluateExn e1 env)

    Exp b e1 ->
      b ^ (evaluateExn e1 env)

    Pow e1 p ->
      (evaluateExn e1 env) ^ p

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

