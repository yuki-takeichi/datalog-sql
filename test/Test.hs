{-# LANGUAGE QuasiQuotes #-}

import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit (testCase)
--import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.HUnit (Assertion, assertBool, (@=?), assertFailure)
--import Test.QuickCheck

import ParserTest

import Data.Map as M
import Data.Datalog.AST
import Data.Datalog.Parser

import Str

main :: IO ()
main = defaultMain tests

pass :: Assertion
pass = assertBool "pass" True

tests :: [Test]
tests = [
          parserTests,
          testGroup "Integration: Transtale datalog code to SQL code" [
          ],
          testGroup "Translate datalog AST to SQL AST" [
            testCase "query with conjunctive rule" test_genSQLASTWithNoRecursiveWithClause,
            testCase "query with recursive and conjunctive rule" undefined
          ],
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


-- ?(him: X) :- ancestor(me: yuki, him: X), grandparent(me: masaki, him: X).
{- select ancestor.him as him
 - from ancestor
 -    , grandparent
 - where ancestor.him = grandparent.him
 -   and ancestor.me = "yuki"
 -   and grandparent.me = "masaki"
 - ;
 -}
test_genSQLAST1 :: Assertion
test_genSQLAST1 = sql @=? genSQLAST M.empty [(queryHead, queryBody)]
  where
    queryHead :: DatalogHead
    queryHead = DatalogHead [ 
                  TupleAttrRef {rel=Relation{name="?", rid=0}, attr="him", arg=Var "X"}
                ]
    queryBody :: DatalogBody
    queryBody = DatalogBody [
                  TupleAttrRef {rel=Relation{name="ancestor", rid=0}, attr="me", arg=Atom "yuki"},
                  TupleAttrRef {rel=Relation{name="ancestor", rid=0}, attr="him", arg=Var "X"},
                  TupleAttrRef {rel=Relation{name="grandparent", rid=0}, attr="me", arg=Atom "masaki"},
                  TupleAttrRef {rel=Relation{name="grandparent", rid=0}, attr="him", arg=Var "X"}
                ]
    sql :: SelectStmt
    sql = SelectStmt {
            withClauses = [],
            selectExprs = [ SelectExpr (ColumnRef "ancestor" "him") (Just "him") ],
            fromTables = [ Table "ancestor" Nothing, Table "grandparent" Nothing ],
            whereClause = [ Equal (ColumnRef "ancestor" "him")   (ColumnRef "grandparent" "him"),
                            Equal (ColumnRef "ancestor" "me")    (SqlStr "yuki"),
                            Equal (ColumnRef "grandparent" "me") (SqlStr "masaki") ]
          }

test_genSQLASTWithNoRecursiveWithClause :: Assertion
test_genSQLASTWithNoRecursiveWithClause = sql @=? genSQLAST (M.fromList [("grandparent", ruleGrandParent)] ) [(queryHead, queryBody)]
  where
    -- grandparent(me: X, him: Y) :- parent(me: X, him: P), parent(me: P, him: Y).
    ruleGrandParent :: DatalogRule
    ruleGrandParent = DatalogRule [ (head, body) ]
      where
        head :: DatalogHead
        head = DatalogHead [
                 TupleAttrRef {rel=Relation{name="grandparent", rid=0}, attr="me", arg=Var "X"},
                 TupleAttrRef {rel=Relation{name="grandparent", rid=0}, attr="him", arg=Var "Y"}
               ]
        body :: DatalogBody
        body = DatalogBody [
                 TupleAttrRef {rel=Relation{name="parent", rid=0}, attr="me", arg=Var "X"},
                 TupleAttrRef {rel=Relation{name="parent", rid=0}, attr="him", arg=Var "P"},
                 TupleAttrRef {rel=Relation{name="parent", rid=1}, attr="me", arg=Var "P"},
                 TupleAttrRef {rel=Relation{name="parent", rid=1}, attr="him", arg=Var "Y"}
               ]

    -- ?(me: X, him: Y) :- grandparent(me: X, him: Y).
    queryHead :: DatalogHead
    queryHead = DatalogHead [ 
                  TupleAttrRef {rel=Relation{name="?", rid=0}, attr="me", arg=Var "X"},
                  TupleAttrRef {rel=Relation{name="?", rid=0}, attr="him", arg=Var "Y"}
                ]

    queryBody :: DatalogBody
    queryBody = DatalogBody [
                  TupleAttrRef {rel=Relation{name="grandparent", rid=0}, attr="me", arg=Atom "X"},
                  TupleAttrRef {rel=Relation{name="grandparent", rid=0}, attr="him", arg=Var "Y"}
                ]
    -- with grandparent(me, him) as (
    --   select parent1.me as me
    --        , parent2.him as him
    --   from parent as parent1
    --        parent as parent2
    --   where true
    --     and parent1.him = parent2.me
    -- )
    -- select grandparent.me as me
    --      , grandparent.him as him
    -- from grandparent
    -- ;
    sql :: SelectStmt
    sql = SelectStmt {
            withClauses = [grandparent],
            selectExprs = [ SelectExpr (ColumnRef "ancestor" "me") (Just "me")
                          , SelectExpr (ColumnRef "ancestor" "him") (Just "him") ],
            fromTables = [ Table "ancestor" Nothing, Table "grandparent" Nothing ],
            whereClause = [ Equal (ColumnRef "ancestor" "him")   (ColumnRef "grandparent" "him"),
                            Equal (ColumnRef "ancestor" "me")    (SqlStr "yuki"),
                            Equal (ColumnRef "grandparent" "me") (SqlStr "masaki") ]
          }
      where
        grandparent :: CTE
        grandparent = CTE "grandparent" SelectStmt {
          withClauses=[],
          selectExprs=[ SelectExpr (ColumnRef "parent1" "me") (Just "me")
                      , SelectExpr (ColumnRef "parent2" "him") (Just "him") ],
          fromTables=[ Table "parent" (Just "parent1"), Table "parent" (Just "parent2") ],
          whereClause=[ Equal (ColumnRef "parent1" "him") (ColumnRef "parent2" "me") ]
        }

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
