module Frac where

import Data.Char (isDigit)

data Frac = Frac Integer Integer
  deriving (Show)

parseFrac :: String -> Frac
parseFrac s =
  case span isDigit s of
    (num, '/':rest)
      | not (null num) ->
          case span isDigit rest of
            (den, "")
              | not (null den) -> Frac (read num) (read den)
            _ -> error "parseFrac invalid syntax"
    _ -> error "parseFrac invalid syntax"
