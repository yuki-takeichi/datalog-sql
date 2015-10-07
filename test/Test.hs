{-# LANGUAGE QuasiQuotes #-}

import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit (testCase)
--import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.HUnit (Assertion, assertBool, (@=?))
--import Test.QuickCheck

import ParserTest
import SQLTranslatorTest

import Data.Map as M
import Data.Datalog.AST

import Str

main :: IO ()
main = defaultMain tests

pass :: Assertion
pass = assertBool "pass" True

tests :: [Test]
tests = [
          testGroup "Integration: Transtale datalog code to SQL code" [
          ],
          parserTests,
          sqlTranslatorTests,
          {-testGroup "query" [
            testCase "always success" pass,
            testCase "genSQL" test_genSQLAST1,
            testCase "genSQLWithNoRecursiveWithClause" test_genSQLASTWithNoRecursiveWithClause,
            testCase "genSQLStringIndented" test_genSQLStringIndented
          ],-}
          testGroup "indent" [
            testCase "indent" test_indent
          ]
        ]



test_indent :: Assertion
test_indent = expected @=? indent is
  where
    is  = Indent 0 [
            Block ["select hoge.foo"],
            Indent 5 [
              Block [", bar.piyo"]
            ],
            Block ["from ("],
            Indent 2 [
              Block ["select foo"],
              Indent 5 [
                Block [", hoge"]
              ],
              Block [
                "from hogehoge",
                "where hogehoge.huga > 10"
              ]
            ],
            Block [") as hoge"],
            Indent 3 [
              Block [", bar"]
            ],
            Block ["where hoge.hage = bar.barbar"],
            Indent 2 [
              Block ["and hoge.hoge = hoge.bar"]
            ],
            Block [";"]
          ]
    expected = [str|select hoge.foo
     , bar.piyo
from (
  select foo
       , hoge
  from hogehoge
  where hogehoge.huga > 10
) as hoge
   , bar
where hoge.hage = bar.barbar
  and hoge.hoge = hoge.bar
;
|]

{-
test_genSQLStringIndented :: Assertion
test_genSQLStringIndented = expected @=? genSQLStringIndented selectStmt
  where
    selectStmt = SelectStmt {
            withClauses = [ grandparent ],
            selectExprs = [
                            SelectExpr (ColumnRef "grandparent" "me") (Just "me"),
                            SelectExpr (ColumnRef "grandparent" "him") (Just "him")
                          ],
            fromTables = [ Table "grandparent" Nothing ],
            whereClause = []
          }
      where
        grandparent :: CTE
        grandparent = CTE "grandparent" SelectStmt {
          withClauses=[],
          selectExprs=[ SelectExpr (ColumnRef "parent1" "me") (Just "me"), SelectExpr (ColumnRef "parent2" "him") (Just "him") ],
          fromTables=[ Table "parent" (Just "parent1"), Table "parent" (Just "parent2") ],
          whereClause=[ Equal (ColumnRef "parent1" "him") (ColumnRef "parent2" "me") ]
        }
    -- with grandparent(me, him) as (
    --   select parent1.me
    --        , parent2.him
    --   from parent as parent1
    --        parent as parent2
    --   where true
    --     and parent1.him = parent2.me
    -- )
    -- select grandparent.me
    --      , grandparent.him
    -- from grandparent
    -- ;
    expected :: IndentedString
    expected = Indent 0 [
                 Block ["with grandparent(me, him) as ("],
                 Indent 2 grandparent,
                 Block [")"],
                 Block ["select grandparent.me"],
                 Indent 5 [
                   Block [", grandparent.him"]
                 ],
                 Block ["from grandparent"]
               ]
      where grandparent = [
                            Block ["select parent1.me"],
                            Indent 5 [
                              Block [", parent2.him"]
                            ],
                            Block ["from parent as parent1"],
                            Indent 3 [
                              Block [", parent as parent2"]
                            ],
                            Block ["where true"],
                            Indent 2 [
                              Block ["and parent1.him = parent2.me"]
                            ]
                          ]
-}
