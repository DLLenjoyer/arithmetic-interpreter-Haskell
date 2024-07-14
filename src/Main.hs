module Main where

import Control.Monad (void)
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.String (Parser)

data Expr
  = Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Num Int
  deriving (Show)

integer :: Parser Int
integer = read <$> many1 digit

expr :: Parser Expr
expr = chainl1 term addop

term :: Parser Expr
term = chainl1 factor mulop

factor :: Parser Expr
factor = parens expr <|> (Num <$> integer)

parens :: Parser a -> Parser a
parens = between (char '(') (char ')')

addop :: Parser (Expr -> Expr -> Expr)
addop = (char '+' >> return Add) <|> (char '-' >> return Sub)

mulop :: Parser (Expr -> Expr -> Expr)
mulop = (char '*' >> return Mul) <|> (char '/' >> return Div)

eval :: Expr -> Int
eval (Num n) = n
eval (Add e1 e2) = eval e1 + eval e2
eval (Sub e1 e2) = eval e1 - eval e2
eval (Mul e1 e2) = eval e1 * eval e2
eval (Div e1 e2) = eval e1 `div` eval e2

main :: IO ()
main = do
  putStrLn "Enter arithmetic expression pls:"
  input <- getLine
  case parse expr "Arithmetic Expression" input of
    Left err -> putStrLn $ "Parse error: " ++ show err
    Right parsedExpr -> do
      putStrLn $ "Parsed expression : " ++ show parsedExpr
      let result = eval parsedExpr
      putStrLn $ "Result is : " ++ show result
