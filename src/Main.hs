import           Data.Char
import qualified Data.Map                 as M
import           System.Console.Haskeline

import           Expr                     as E
import           Parser.Core
import           Parser.Statement


main :: IO ()
main = runInputT defaultSettings $ promptLoop (Context M.empty M.empty)

promptLoop :: Context -> InputT IO ()
promptLoop context = do
    minput <- getInputLine "> "
    case minput of
        Nothing     -> return ()
        Just "exit" -> return ()
        Just "env"  -> putEnv context >>= promptLoop

        Just input  -> do
            case runParserStrict statementP (filter (not . isSpace) input) of
                 Left err -> outputStrLn ("Error parsing: " ++ err) >> promptLoop context
                 Right s  -> Main.eval s context >>= promptLoop


eval :: Statement -> Context -> InputT IO Context

eval (Evaluation e) c = do case E.eval e c of
                             Just evaluated -> outputStrLn $ show evaluated
                             Nothing        -> outputStrLn "Error: couldn't evaluate expression"
                           return c

eval (VariableDeclaration name value) (Context vars funcs) =
    case E.eval value context of
        Just e  -> do outputStrLn $ show e
                      return $ Context (M.insert name e vars) funcs
        Nothing -> outputStrLn "Error: couldn't evaluate expression" >> return context
    where context = Context vars funcs

eval (FunctionDeclaration name argName e) (Context vars funcs) =
    -- case evalIgnore e context argName of
    --     Just e  -> do outputStrLn $ show e
    --                   return $ Context vars (M.insert name (argName, e) funcs)
    --     Nothing -> outputStrLn "Error: couldn't evaluate expression" >> return context
    --
    -- where context = Context vars funcs
    return $ Context vars (M.insert name (argName, e) funcs)

eval _ c = return c
-- eval (PolynomEvaluation left right) c = do l <- eval left  -- count number of unknoewn
--                                            r <- eval right



putEnv :: Context -> InputT IO Context
putEnv (Context vars funcs) = do
    outputStrLn "Variables:"
    outputStr $ M.foldrWithKey (foldFunc fmtVariable) "" vars
    outputStrLn "Functions:"
    outputStr $ M.foldrWithKey (foldFunc fmtFunction) "" funcs
    return (Context vars funcs)
    where foldFunc fmt k v acc   = acc ++ fmt k v ++ "\n"
          fmtVariable k v        = k ++ " = " ++ show v
          fmtFunction k (arg, v) = k ++ "(" ++ arg ++ ") = " ++ show v
