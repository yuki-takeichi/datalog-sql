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
                         testCase "Simple" test_genSQLASTRule,
                         testCase "Disjunctive" test_genSQLASTRuleDisjunctive
                       ],
                       testGroup "Query" [
                         testCase "Simple" test_genSQLASTQuery,
                         testCase "With conjunctive rule" test_genSQLASTQueryNoRecursive,
                         testCase "With recursive and conjunctive rule" test_genSQLASTQueryRecursive
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

grandparentView :: SelectStmt
grandparentView = SelectStmt {
  withClauses=[],
  selectExprs=[ SelectExpr (ColumnRef "parent" "me") (Just "me")
              , SelectExpr (ColumnRef "parent1" "him") (Just "him") ],
  fromTables=[ Table "parent" Nothing, Table "parent" (Just "parent1") ],
  whereClause=[ Equal (ColumnRef "parent" "him") (ColumnRef "parent1" "me") ]
}

test_genSQLASTRule :: Assertion
test_genSQLASTRule = grandparentView @=? genSQLAST M.empty ruleGrandParent

-- parent(me: X, him: Y) :- father(me: X, him: Y).
-- parent(me: X, him: Y) :- mother(me, X, him: Y).
ruleParent :: DatalogStmt
ruleParent = DatalogRule [ (fHead, fBody), (mHead, mBody) ]
  where
    fHead :: DatalogHead
    fHead = DatalogHead [ 
                  TupleAttrRef {rel=Relation{name="parent", rid=0}, attr="me", arg=Var "X"},
                  TupleAttrRef {rel=Relation{name="parent", rid=0}, attr="him", arg=Var "Y"}
                ]
    fBody :: DatalogBody
    fBody = DatalogBody [
                  TupleAttrRef {rel=Relation{name="father", rid=0}, attr="me", arg=Var "X"},
                  TupleAttrRef {rel=Relation{name="father", rid=0}, attr="him", arg=Var "Y"}
                ]
    mHead :: DatalogHead
    mHead = DatalogHead [ 
                  TupleAttrRef {rel=Relation{name="parent", rid=0}, attr="me", arg=Var "X"},
                  TupleAttrRef {rel=Relation{name="parent", rid=0}, attr="him", arg=Var "Y"}
                ]
    mBody :: DatalogBody
    mBody = DatalogBody [
                  TupleAttrRef {rel=Relation{name="mother", rid=0}, attr="me", arg=Var "X"},
                  TupleAttrRef {rel=Relation{name="mother", rid=0}, attr="him", arg=Var "Y"}
                ]


-- select father.me as me
--      , father.him as him
-- from father
-- union
-- select mother.me as me
--      , mother.him as him
-- from mother
parentView :: SelectStmt
parentView = Union father mother
  where
    father :: SelectStmt
    father = SelectStmt {
            withClauses = [],
            selectExprs = [ SelectExpr (ColumnRef "father" "me") (Just "me")
                          , SelectExpr (ColumnRef "father" "him") (Just "him") ],
            fromTables = [ Table "father" Nothing ],
            whereClause = []
          }
    mother :: SelectStmt
    mother = SelectStmt {
            withClauses = [],
            selectExprs = [ SelectExpr (ColumnRef "mother" "me") (Just "me")
                          , SelectExpr (ColumnRef "mother" "him") (Just "him") ],
            fromTables = [ Table "mother" Nothing ],
            whereClause = []
          }

test_genSQLASTRuleDisjunctive :: Assertion
test_genSQLASTRuleDisjunctive = parentView @=? genSQLAST M.empty ruleParent

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

test_genSQLASTQueryNoRecursive :: Assertion
test_genSQLASTQueryNoRecursive = sql @=? genSQLAST (M.fromList [("grandparent", ruleGrandParent)] ) (DatalogQuery queryHead queryBody)
  where
    -- ?(me: X, him: Y) :- grandparent(me: X, him: Y).
    queryHead :: DatalogHead
    queryHead = DatalogHead [ 
                  TupleAttrRef {rel=Relation{name="?", rid=0}, attr="me", arg=Var "X"},
                  TupleAttrRef {rel=Relation{name="?", rid=0}, attr="him", arg=Var "Y"}
                ]

    queryBody :: DatalogBody
    queryBody = DatalogBody [
                  TupleAttrRef {rel=Relation{name="grandparent", rid=0}, attr="me", arg=Var "X"},
                  TupleAttrRef {rel=Relation{name="grandparent", rid=0}, attr="him", arg=Var "Y"}
                ]
    -- with grandparent(me, him) as (
    --   select parent.me as me
    --        , parent1.him as him
    --   from parent
    --        parent as parent1
    --   where true
    --     and parent.him = parent1.me
    -- )
    -- select grandparent.me as me
    --      , grandparent.him as him
    -- from grandparent
    -- ;
    sql :: SelectStmt
    sql = SelectStmt {
            withClauses = [ CTE "grandparent" grandparentView ],
            selectExprs = [ SelectExpr (ColumnRef "grandparent" "me") (Just "me")
                          , SelectExpr (ColumnRef "grandparent" "him") (Just "him") ],
            fromTables = [ Table "grandparent" Nothing ],
            whereClause = []
          }

-- ancestor(me: X, him: Y) :- parent(me: X, him: Y) ;
-- ancestor(me: X, him: Y) :- parent(me: X, him: P), ancestor(me: P, him:Y).
ruleAncestor :: DatalogStmt
ruleAncestor = DatalogRule [ (baseHead, baseBody), (recHead, recBody) ]
  where
    baseHead :: DatalogHead
    baseHead = undefined
    baseBody :: DatalogBody
    baseBody = undefined
    recHead :: DatalogHead
    recHead = undefined
    recBody :: DatalogBody
    recBody = undefined

-- with recursive ancestor (
--   select parent.me as me
--        , parent.him as him
--   from parent
--   union all
--   select parent.me as me
--        , ancestor.him as him
--   from parent
--      , ancestor
-- )
ancestorView :: CTE
ancestorView = CTE "ancestor" SelectStmt {
  withClauses = [],
  selectExprs = [],
  fromTables = [],
  whereClause = []
}

-- select ancestor.me as him
--      , ancestor.him as him
-- from ancestor
test_genSQLASTQueryRecursive :: Assertion
test_genSQLASTQueryRecursive = sql @=? genSQLAST (M.fromList [("ancestor", ruleAncestor)]) query
  where
    -- ?(me: X, him: Y) :- ancestor(me: X, him: Y).
    query :: DatalogStmt
    query = DatalogQuery queryHead queryBody
    queryHead :: DatalogHead
    queryHead = DatalogHead [ 
                  TupleAttrRef {rel=Relation{name="?", rid=0}, attr="me", arg=Var "X"},
                  TupleAttrRef {rel=Relation{name="?", rid=0}, attr="him", arg=Var "Y"}
                ]
    queryBody :: DatalogBody
    queryBody = DatalogBody [
                  TupleAttrRef {rel=Relation{name="ancestor", rid=0}, attr="me", arg=Var "X"},
                  TupleAttrRef {rel=Relation{name="ancestor", rid=0}, attr="him", arg=Var "Y"}
                ]
    sql :: SelectStmt
    sql = SelectStmt {
            withClauses = [ ancestorView ],
            selectExprs = [ SelectExpr (ColumnRef "ancestor" "me") (Just "me")
                          , SelectExpr (ColumnRef "ancestor" "him") (Just "him") ],
            fromTables = [ Table "ancestor" Nothing ],
            whereClause = []
          }
