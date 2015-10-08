{-# LANGUAGE QuasiQuotes #-}

import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit (testCase)
--import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.HUnit (Assertion, assertBool, (@=?))
--import Test.QuickCheck

import ParserTest
import SQLTranslatorTest

--import Data.Map as M
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
          testGroup "Generate SQL AST to SQL code" [
            testCase "Simple" test_generatorSimple,
            testCase "With Clause" test_generatorWithClause
          ],
          testCase "Indent" test_indent
        ]



test_indent :: Assertion
test_indent = expected @=? indent is
  where
    is  = Indent 0 [
            Line "select hoge.foo",
            Indent 5 [
              Line ", bar.piyo"
            ],
            Line "from (",
            Indent 2 [
              Line "select foo",
              Indent 5 [
                Line ", hoge"
              ],
              Line "from hogehoge",
              Line "where hogehoge.huga > 10"
            ],
            Line ") as hoge",
            Indent 3 [
              Line ", bar"
            ],
            Line "where hoge.hage = bar.barbar",
            Indent 2 [
              Line "and hoge.hoge = hoge.bar"
            ],
            Line ";"
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

test_generatorSimple :: Assertion
test_generatorSimple = expected @=? generateSQLCode stmt
  where
    stmt = SelectStmt {
             withClauses = [],
             selectExprs = [SelectExpr (ColumnRef "ancestor" "me") (Just "me"),SelectExpr (ColumnRef "ancestor" "him") (Just "him")],
             fromTables = [Table "ancestor" Nothing],
             whereClause = [ Equal (ColumnRef "ancestor" "me") (SqlStr "yuki") ]
           }
    expected = Indent 0 [
                 Line "select ancestor.me as me",
                 Indent 5 [
                   Line ", ancestor.him as him"
                 ],
                 Line "from ancestor",
                 Line "where true",
                 Indent 2 [
                   Line "and ancestor.me = \"yuki\""
                 ],
                 Line ";"
               ]

ancestorView :: CTE
ancestorView = CTE "ancestor" $ UnionAll base rec
  where
    base = SelectStmt {
      withClauses = [],
      selectExprs = [ SelectExpr (ColumnRef "parent" "me") (Just "me")
                    , SelectExpr (ColumnRef "parent" "him") (Just "him") ],
      fromTables = [ Table "parent" Nothing ],
      whereClause = []
    }
    rec = SelectStmt {
      withClauses = [],
      selectExprs = [ SelectExpr (ColumnRef "parent" "me") (Just "me")
                    , SelectExpr (ColumnRef "ancestor" "him") (Just "him") ],
      fromTables = [ Table "parent" Nothing, Table "ancestor" Nothing ],
      whereClause=[ Equal (ColumnRef "parent" "him") (ColumnRef "ancestor" "me") ]
    }

test_generatorWithClause :: Assertion
test_generatorWithClause = expected @=? generateSQLCode stmt
  where
    stmt = SelectStmt {
             withClauses = [ ancestorView ],
             selectExprs = [SelectExpr (ColumnRef "ancestor" "me") (Just "me"),SelectExpr (ColumnRef "ancestor" "him") (Just "him")],
             fromTables = [Table "ancestor" Nothing],
             whereClause = [ Equal (ColumnRef "ancestor" "me") (SqlStr "yuki") ]
           }
    expected = Indent 0 [
                 Line "with recursive ancestor (",
                 Indent 2 [
                   Line "select parent.me as me",
                   Indent 5 [
                     Line ", parent.him as him"
                   ],
                   Line "from parent",
                   Line "union all",
                   Line "select parent.me as me",
                   Indent 5 [
                     Line ", ancestor.him as him"
                   ],
                   Line "from parent",
                   Indent 3 [
                     Line ", ancestor"
                   ],
                   Line "where true",
                   Indent 2 [
                     Line "and parent.him = ancestor.me"
                   ]
                 ],
                 Line ")",
                 Line "select ancestor.me as me",
                 Indent 5 [
                   Line ", ancestor.him as him"
                 ],
                 Line "from ancestor",
                 Line "where true",
                 Indent 2 [
                   Line "and ancestor.me = \"yuki\""
                 ],
                 Line ";"
               ]
