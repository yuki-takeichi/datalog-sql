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
            testCase "indent" test_indent,
            testCase "simple" test_generatorSimple
          ]
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
             whereClause = []
           }
    expected = Indent 0 [
                 Line "select ancestor.me as me",
                 Indent 5 [
                   Line ", ancestor.him as him"
                 ],
                 Line "from ancestor",
                 Line ";"
               ]
