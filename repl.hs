--import Data.Datalog
--import Data.Datalog.AST
--import Language.Datalog.Translator.SQL

import Database.HDBC
import Database.HDBC.PostgreSQL

main :: IO ()
main = withPostgreSQL "host=localhost dbname=test" (\conn ->
        do runRaw conn "select * from parent;"
           putStrLn "hoge"
           return ()
       )

