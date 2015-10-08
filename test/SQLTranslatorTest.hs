module SQLTranslatorTest (
  sqlTranslatorTests
) where

import Test.Framework (testGroup, Test)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (Assertion, (@=?))

import Data.Datalog.AST
import Language.Datalog.Translator.SQL 
import Data.Map as M

sqlTranslatorTests :: Test
sqlTranslatorTests = testGroup "Translate datalog AST to SQL AST" [
                       testGroup "Rule" [
                         testCase "Simple" test_genSQLASTRule
                       ],
                       testGroup "Query" [
                         testCase "Simple" test_genSQLASTQuery,
                         testCase "query with conjunctive rule" test_genSQLASTWithNoRecursiveWithClause,
                         testCase "query with recursive and conjunctive rule" undefined
                       ]
                     ]

-- grandparent(me: X, him: Y) :- parent(me: X, him: P), parent(me: P, him: Y).
ruleGrandParent :: DatalogStmt
ruleGrandParent = DatalogRule [ (rhead, rbody) ]
  where
    rhead :: DatalogHead
    rhead = DatalogHead [
              TupleAttrRef {rel=Relation{name="grandparent", rid=0}, attr="me", arg=Var "X"},
              TupleAttrRef {rel=Relation{name="grandparent", rid=0}, attr="him", arg=Var "Y"}
            ]
    rbody :: DatalogBody
    rbody = DatalogBody [
              TupleAttrRef {rel=Relation{name="parent", rid=0}, attr="me", arg=Var "X"},
              TupleAttrRef {rel=Relation{name="parent", rid=0}, attr="him", arg=Var "P"},
              TupleAttrRef {rel=Relation{name="parent", rid=1}, attr="me", arg=Var "P"},
              TupleAttrRef {rel=Relation{name="parent", rid=1}, attr="him", arg=Var "Y"}
            ]

test_genSQLASTRule :: Assertion
test_genSQLASTRule = sql @=? genSQLAST M.empty ruleGrandParent
  where
    sql :: SelectStmt
    sql = SelectStmt {
      withClauses=[],
      selectExprs=[ SelectExpr (ColumnRef "parent" "me") (Just "me")
                  , SelectExpr (ColumnRef "parent1" "him") (Just "him") ],
      fromTables=[ Table "parent" Nothing, Table "parent" (Just "parent1") ],
      whereClause=[ Equal (ColumnRef "parent" "him") (ColumnRef "parent1" "me") ]
    }

-- ?(him: X) :- ancestor(me: yuki, him: X), grandparent(me: masaki, him: X).
{- select ancestor.him as him
 - from ancestor
 -    , grandparent
 - where ancestor.him = grandparent.him
 -   and ancestor.me = "yuki"
 -   and grandparent.me = "masaki"
 - ;
 -}
test_genSQLASTQuery :: Assertion
test_genSQLASTQuery = sql @=? genSQLAST M.empty (DatalogQuery queryHead queryBody)
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
test_genSQLASTWithNoRecursiveWithClause = sql @=? genSQLAST (M.fromList [("grandparent", ruleGrandParent)] ) (DatalogQuery queryHead queryBody)
  where
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
