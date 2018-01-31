module Parser  where

import Data.Char (toLower, toUpper, isDigit)
import Control.Applicative (Alternative(..))

data Parser a = P (String -> [(a, String)])

runParser :: Parser a -> String -> [(a, String)]
runParser (P f) s = f s

pfail :: Parser a
pfail = P (\_ -> [])

item :: Parser Char
item = P (\s -> case s of
                  [] -> []
                  x:xs -> [(x, xs)]
         )

instance Functor Parser where
  fmap f p = P (\s -> [ (f x, s') | (x, s') -> runParser p s])

instance Applicative Parser where
  pure x = P (\s -> [(x, s)])

  p1 <*> p2 = P (\s -> [ (f x, s2) | (f, s1) <- runParser p1 s,
                                     (x, s2) <- runParser p2 s1
                       ]
                )

instance Alternative Parser where
  p1 <|> p2 = P (\s -> runParser p1 s ++ runParser p2 s)
  
satisfy :: (a -> Bool) -> Parser a -> Parser a
satisfy test p = P (\s -> filter (test . fst) (runParser p s))

char :: Char -> Parser Char
char x = satisfy (==x) item

parseInt :: Parser Integer
parseInt = fmap read (some (satisfy isDigit item))

parseFrac :: Parser (Integer, Integer)
parseFrac = pure (\n _ d -> (n, d)) <*> parseInt <*> char '/' <*> parseInt

