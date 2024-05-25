module Main where

import Data.Char

expr :: String -> IO ()
expr [] = print ""
expr (x : xs) = do
    digit x
    expr' xs

digit :: Char -> IO ()
digit lookahead = if isDigit lookahead then print lookahead else error "syntax error"

expr' :: String -> IO ()
expr' [] = print ""
expr' (x : xs) = do
 case x of
  '+' -> do
   (digit . head) xs
   print '+'
   (expr' . tail) xs
  '-' -> do
   (digit . head) xs
   print '-'
   (expr' . tail) xs
  ' ' -> print " "
  _ -> error "syntax error"

translate :: String -> IO ()
translate = expr
main :: IO ()
main = translate "5+2-8+1-2+7+3"




