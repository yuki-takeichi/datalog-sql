{-# LANGUAGE QuasiQuotes #-}
module ParserTest (
  parserTests
) where

import Test.Framework (testGroup, Test)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (Assertion, (@=?), assertFailure)
import Str

import Data.Datalog.Parser
import Data.Datalog.AST

parserTests :: Test
parserTests = testGroup "Parse datalog source code to AST" [
                testGroup "Query" [
                  testCase "Simple" test_query
                ],
                testGroup "Rule" [
                  testCase "Conjinctive" test_rule,
                  testCase "Conjinctive & Disjunctive" test_ruleDisjunctive
                ]
              ]
testParser :: String -> DatalogStmt -> Assertion
testParser code expected = case parse "(test)" code of
                             Left e -> assertFailure $ show e
                             Right actual -> expected @=? actual

test_query :: Assertion
test_query = testParser datalog expected
  where 
    datalog  = [str|?(ans:X):-grandparent(me:yuki,him:X).|]
    expected = DatalogQuery (
                 DatalogHead [
                   TupleAttrRef {rel=Relation{name="?", rid=0}, attr="ans", arg=Var "X"}
                 ]
               ) (
                 DatalogBody [
                   TupleAttrRef {rel=Relation{name="grandparent", rid=0}, attr="me", arg=Atom "yuki"},
                   TupleAttrRef {rel=Relation{name="grandparent", rid=0}, attr="him", arg=Var "X"}
                 ]
               )

test_rule :: Assertion
test_rule = testParser datalog expected
  where
    datalog  = [str|grandparent(me:X,him:Y):-parent(me:X,him:P),parent(me:P,him:Y).|]
    expected = DatalogRule [
                 (DatalogHead [
                   TupleAttrRef {rel=Relation{name="grandparent", rid=0}, attr="me", arg=Var "X"},
                   TupleAttrRef {rel=Relation{name="grandparent", rid=0}, attr="him", arg=Var "Y"}
                  ],
                  DatalogBody [
                    TupleAttrRef {rel=Relation{name="parent", rid=0}, attr="me", arg=Var "X"},
                    TupleAttrRef {rel=Relation{name="parent", rid=0}, attr="him", arg=Var "P"},
                    TupleAttrRef {rel=Relation{name="parent", rid=1}, attr="me", arg=Var "P"},
                    TupleAttrRef {rel=Relation{name="parent", rid=1}, attr="him", arg=Var "Y"}
                  ]
                 )
               ]

test_ruleDisjunctive :: Assertion
test_ruleDisjunctive = testParser datalog expected
  where
    datalog  = [str|parent(me:X,him:Y):-father(me:X,him:Y);parent(me:X,him:Y):-mother(me:X,him:Y).|]
    expected = DatalogRule [
                 (DatalogHead [
                   TupleAttrRef {rel=Relation{name="parent", rid=0}, attr="me", arg=Var "X"},
                   TupleAttrRef {rel=Relation{name="parent", rid=0}, attr="him", arg=Var "Y"}
                  ],
                  DatalogBody [
                    TupleAttrRef {rel=Relation{name="father", rid=0}, attr="me", arg=Var "X"},
                    TupleAttrRef {rel=Relation{name="father", rid=0}, attr="him", arg=Var "Y"}
                  ]
                 ),
                 (DatalogHead [
                   TupleAttrRef {rel=Relation{name="parent", rid=0}, attr="me", arg=Var "X"},
                   TupleAttrRef {rel=Relation{name="parent", rid=0}, attr="him", arg=Var "Y"}
                  ],
                  DatalogBody [
                    TupleAttrRef {rel=Relation{name="mother", rid=0}, attr="me", arg=Var "X"},
                    TupleAttrRef {rel=Relation{name="mother", rid=0}, attr="him", arg=Var "Y"}
                  ]
                 )
               ]
