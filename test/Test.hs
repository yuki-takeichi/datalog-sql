import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit (testCase)
--import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.HUnit (Assertion, assertBool, (@=?))
--import Test.QuickCheck

import Data.Map
import Data.Datalog.AST


main :: IO ()
main = defaultMain tests

pass :: Assertion
pass = assertBool "pass" True

tests :: [Test]
tests = [
        testGroup "query" [
                testCase "always success" pass,
                testCase "genSQL" test_genSQLAST1
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
test_genSQLAST1 = sql @=? genSQLAST empty [(queryHead, queryBody)]
  where
    queryHead :: DatalogHead
    queryHead = DatalogHead [ 
                  TupleAttrRef {fact="?", attr="him", arg=Var "X"}
                ]
    queryBody :: DatalogBody
    queryBody = DatalogBody [
                  TupleAttrRef {fact="ancestor", attr="me", arg=Atom "yuki"},
                  TupleAttrRef {fact="ancestor", attr="him", arg=Var "X"},
                  TupleAttrRef {fact="grandparent", attr="me", arg=Atom "masaki"},
                  TupleAttrRef {fact="grandparent", attr="him", arg=Var "X"}
                ]
    sql :: SelectStmt
    sql = SelectStmt {
            withClauses = [],
            selectExprs = [ SelectExpr (ColumnRef "ancestor" "him") (Just "him") ],
            fromTables = [ Table "ancestor", Table "grandparent" ],
            whereClause = [ Equal (ColumnRef "ancestor" "him")   (ColumnRef "grandparent" "him"),
                            Equal (ColumnRef "ancestor" "me")    (SqlStr "yuki"),
                            Equal (ColumnRef "grandparent" "me") (SqlStr "masaki") ]
          }
