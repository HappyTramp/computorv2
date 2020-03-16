import System.IO
import Statement
import Parser.Statement
import Parser.Core
import Statement
import Assignment
import Expr
import Evaluation


main = promptLoop []

promptLoop :: Context -> IO ()
promptLoop context = do
    line <- prompt
    if line /= "exit"
       then loop line context >>= promptLoop
       else return ()

loop :: String -> Context -> IO Context
loop line context = do s <- parseIO line
                       context <- printStatement s context
                       putStrLn $ show context
                       return context

prompt :: IO String
prompt = do putStr "> "
            hFlush stdout
            getLine

parseIO :: String -> IO Statement
parseIO input = case parseStrict statementP input of
                  Nothing -> fail "Couldn't parse input"
                  Just s  -> return s

printStatement :: Statement -> Context -> IO Context
printStatement (SAssignment a) context = do putStrLn $ show a
                                            return $ update context a
printStatement (SExpr e) context = do putStrLn evalStr
                                      return context
    where evalStr = case eval context e of Nothing -> "Couldn't evaluate expression"
                                           Just a  -> show a
