import           Data.Char
import qualified Data.Map         as M
import           System.IO

import           Expr             as E
import           Parser.Core
import           Parser.Statement


main :: IO ()
main = promptLoop (Context M.empty M.empty)

promptLoop :: Context -> IO ()
promptLoop context = do
    putStr "> "
    hFlush stdout
    eof <- isEOF
    if eof
        then return ()
        else do line <- getLine
                if line /= "exit"
                   then loop line context >>= promptLoop
                   else return ()

loop :: String -> Context -> IO Context
loop input context =
    do
        case runParserStrict statementP (filter (not . isSpace) input) of
            Left err -> putStrLn ("Error parsing: " ++ err) >> return context
            Right s  -> Main.eval s context



eval :: Statement -> Context -> IO Context

eval (Evaluation e) c = do case E.eval e c of
                             Just evaluated -> putStrLn $ show evaluated
                             Nothing        -> putStrLn "Error: couldn't evaluate expression"
                           return c

eval (VariableDeclaration name value) (Context vars funcs) =
    case E.eval value context of
        Just e  -> do putStrLn $ show e
                      return $ Context (M.insert name e vars) funcs
        Nothing -> putStrLn "Error: couldn't evaluate expression" >> return context
    where context = Context vars funcs

eval (FunctionDeclaration name argName e) (Context vars funcs) =
    -- case evalIgnore e context argName of
    --     Just e  -> do putStrLn $ show e
    --                   return $ Context vars (M.insert name (argName, e) funcs)
    --     Nothing -> putStrLn "Error: couldn't evaluate expression" >> return context
    --
    -- where context = Context vars funcs
    return $ Context vars (M.insert name (argName, e) funcs)

eval _ c = return c
-- eval (PolynomEvaluation left right) c = do l <- eval left  -- count number of unknoewn
--                                            r <- eval right


