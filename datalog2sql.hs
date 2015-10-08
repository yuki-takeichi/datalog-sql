-- FOR LIVE DEMO USE ONLY
-- TODO Refactor later
import Data.Map as M

import Data.Datalog.AST
import Data.Datalog.Parser
import Language.Datalog.Translator.SQL 

main :: IO ()
main = do cts <- getContents
          putStr $ translator $ lines $ cts

translator :: [String] -> String
translator [] = []
translator (query:[]) = case parse "(test)" query of
                            Left e -> show e
                            Right dquery -> (indent $ generateSQLCode $ genSQLAST M.empty dquery)
translator (rule:(query:[])) = case parse "(test)" rule of
                            Left e -> show e
                            Right drule@(DatalogRule ((DatalogHead h, DatalogBody _):_)) -> let ruleName = name $ rel $ h!!0
                                                      in case parse "(test)" query of
                                                        Left e -> show e
                                                        Right dquery -> (indent $ generateSQLCode $ genSQLAST (M.fromList [(ruleName, drule)]) dquery)
                            Right _ -> error "Datalog rule required"
translator (rule1:(rule2:(query:[]))) = case parse "(test)" rule1 of
                            Left e -> show e
                            Right drule1@(DatalogRule ((DatalogHead h, DatalogBody _):_)) -> let ruleName1 = name $ rel $ h!!0
                                    in case parse "(test)" rule2 of
                                      Left e -> show e
                                      Right drule2@(DatalogRule ((DatalogHead h, DatalogBody _):_)) -> let ruleName2 = name $ rel $ h!!0 
                                            in case parse "(test)" query of
                                              Left e -> show e
                                              Right dquery -> (indent $ generateSQLCode $ genSQLAST (M.fromList [(ruleName1, drule1),(ruleName2, drule2)]) dquery)
                            Right _ -> error "Datalog rule required"
