module Expression.Parse where

import Native.ParseExpression
import Expression.Type exposing (Expression(..))

parse : String -> Result String Expression
parse = Native.ParseExpression.parse
