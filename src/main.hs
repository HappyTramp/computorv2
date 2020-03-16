import System.IO
import Statement
import Parser.Statement
import Parser.Core


main = do
    line <- prompt
    loop line

loop :: String -> IO ()
loop "exit" = return ()
loop line   = do s <- parseIO line
                 putStrLn $ show s
                 main

prompt :: IO String
prompt = do putStr "> "
            hFlush stdout
            getLine

parseIO :: String -> IO Statement
parseIO input = case parseStrict statementP input of
                  Nothing -> fail "Couldn't parse input"
                  Just s  -> return s
