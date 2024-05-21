module Main where

digitFIRST :: [Char]
digitFIRST = [ '0', '1', '2', '3', '4', '5', '6', '7', '8', '9' ]


expr :: String -> IO ()
expr (lookahead : xs) = do
    digit lookahead
    expr' xs
expr [] = return () -- understand


digit :: Char -> IO ()
digit lookahead = if lookahead `elem` digitFIRST
                     then print lookahead
                     else error "syntax error"

expr' :: String -> IO ()
expr' (lookahead : xs) = do
    case lookahead of
         '+' -> do
             digit (head xs)
             print '+'
             expr' (tail xs)
         '-' -> do
             digit (head xs)
             print '-'
             expr' (tail xs)
         ' ' -> print " "
         _ -> error "syntax error"

expr' [] = return ()

translate :: String -> IO ()
translate = expr
main :: IO ()
main = translate "5+2-8+1-2+7+3"




