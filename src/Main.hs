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

eval (Evaluation e) c = evalOutput (E.eval e c) c (id . const)

eval (VariableDeclaration name value) c = evalOutput (E.eval value c) c nextContext
    where nextContext (Context vs fs) e = Context (M.insert name e vs) fs

eval (FunctionDeclaration name argName value) c = evalOutput (evalIgnored value c argName) c nextContext
    where nextContext (Context vs fs) e = Context vs (M.insert name (argName, e) fs)

eval _ c = return c
-- eval (PolynomEvaluation left right) c = do l <- eval left  -- count number of unknoewn
--                                            r <- eval right


evalOutput :: Maybe Expr -> Context -> (Context -> Expr -> Context) -> InputT IO Context
evalOutput (Just evaluated) c f = do outputStrLn $ show evaluated
                                     return $ f c evaluated
evalOutput Nothing c _ = outputStrLn "Error: couldn't evaluate expression" >> return c


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
