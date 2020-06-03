import           Data.Char
import qualified Data.Map         as M
import           System.IO

import           Evaluation       as E
import           Expr
import           Parser.Core
import           Parser.Expr
import           Parser.Statement


main :: IO ()
main = promptLoop (Context M.empty M.empty)

promptLoop :: Context -> IO ()
promptLoop context = do
    putStr "> "
    hFlush stdout
    line <- getLine
    return ()
    if line /= "q"
       then loop line context >>= promptLoop
       else return ()

loop :: String -> Context -> IO Context
loop input context =
    do
        statement <- case runParserStrict statementP (filter (not . isSpace) input) of
                        Left err -> fail ("Error: " ++ err)
                        Right s  -> return s
        Main.eval context statement


eval :: Context -> Statement -> IO Context

eval c (Evaluation e) = do case E.eval c e of
                             Just evaluated -> putStrLn $ show evaluated
                             Nothing        -> putStrLn "Error: couldn't evaluate expression"
                           return c

eval (Context vars funcs) (VariableDeclaration name e) =
    case E.eval context e of
        Just evaluated -> return $ Context (M.insert name e vars) funcs
        Nothing        -> putStrLn "Error: couldn't evaluate expression" >> return context
    where context = Context vars funcs

eval (Context vars funcs) (FunctionDeclaration name argName e) =
    -- case E.eval context e of
        -- Just evaluated -> return $ Context vars (M.insert name (argName, e) funcs)
        -- Nothing        -> putStrLn "Error: couldn't evaluate expression" >> return context
        return $ Context vars (M.insert name (argName, e) funcs)
    -- where context = Context vars funcs
