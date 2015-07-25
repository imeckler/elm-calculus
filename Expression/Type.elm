module Expression.Type where

type Expression
  = Var String
  | Constant Float
  | Mul Expression Expression
  | Add Expression Expression
  | LogBase Float Expression
  | Exp Float Expression
  | Pow Expression Float

