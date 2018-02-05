module Parser  where

import Data.Char (toLower, toUpper, isDigit, isSpace)
import Control.Applicative (Alternative(..))

newtype Parser a = P (String -> [(a, String)])

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
  fmap f p = P (\s -> [ (f x, s') | (x, s') <- runParser p s])

instance Applicative Parser where
  pure x = P (\s -> [(x, s)])

  p1 <*> p2 = P (\s -> [ (f x, s2) | (f, s1) <- runParser p1 s,
                                     (x, s2) <- runParser p2 s1
                       ]
                )

instance Alternative Parser where
  empty = pfail
  
  p1 <|> p2 = P (\s -> runParser p1 s ++ runParser p2 s)

instance Monad Parser where
  p1 >>= f = P (\s -> [ (y, s2) | (x, s1) <- runParser p1 s,
                                  (y, s2) <- runParser (f x) s1
                      ]
               )
  
satisfy :: (a -> Bool) -> Parser a -> Parser a
satisfy test p = P (\s -> [ (x, s') | (x, s') <- runParser p s, test x ])

char :: Char -> Parser Char
char x = satisfy (==x) item

space :: Parser Char
space = satisfy isSpace item

parseInt :: Parser Integer
parseInt = fmap read (some (satisfy isDigit item))

parseFrac :: Parser (Integer, Integer)
--parseFrac = pure (\n _ d -> (n, d)) <*> parseInt <*> char '/' <*> parseInt
parseFrac = do n <- parseInt
               char '/'
               d <- parseInt
               return (n, d)

token :: Parser a -> Parser a
token p = many space *> p



data Exp
  = Cte Integer
  | Bin Op Exp Exp
  deriving (Show)

data Op
  = Add
  | Sub
  | Mul
  | Div
  deriving (Show)

factor = fmap Cte parseInt
         <|>
         char '(' *> expression <* char ')'

term = chainl1 factor (multiplication <|> division)

expression = chainl1 term (addition <|> subtraction)

addition       = char '+' *> pure (Bin Add)
subtraction    = char '-' *> pure (Bin Sub)
multiplication = char '*' *> pure (Bin Mul)
division       = char '/' *> pure (Bin Div)

chainl p op = chainl1 p op <|> empty

chainl1 p op = p >>= aux
  where
    aux x = do f <- op
               y <- p
               aux (f x y)
            <|>
            pure x
