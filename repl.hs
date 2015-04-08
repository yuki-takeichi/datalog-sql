import Data.Datalog
import Text.Parsec

main :: IO ()
main = do eitherFacts <- fromSource "./takeichi.datalog"
          case eitherFacts of
            Left _ -> return ()
            Right fs -> repl fs

repl :: Facts -> IO ()
repl fs = do putStr "?- "
             line <- getLine
             case parse query "" line of
               Left _ -> return ()
               Right q -> putStrLn $ show $ eval fs q
             putStrLn ""
             repl fs
