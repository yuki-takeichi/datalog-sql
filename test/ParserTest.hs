{-# LANGUAGE QuasiQuotes #-}
module ParserTest (
  parserTests
) where

import Test.Framework (testGroup, Test)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (Assertion, assertBool, (@=?), assertFailure)
import Str

import Text.Parsec
import Data.Datalog.Parser
import Data.Datalog.AST

parserTests :: Test
parserTests = testGroup "Parse datalog source code to AST" [
                testCase "query" test_query
              ]

test_query :: Assertion
test_query = case parse query "(test)" datalogString of
               Left e -> assertFailure $ show e
               Right actual -> expected @=? actual
  where 
    datalogString = [str|?(ans:X):-grandfather(me:yuki,him:X).|]
    expected = DatalogQuery (
                 DatalogHead [
                   TupleAttrRef {rel=Relation{name="?", rid=0}, attr="ans", arg=Var "X"}
                 ]
               ) (
                 DatalogBody [
                   TupleAttrRef {rel=Relation{name="grandfather", rid=0}, attr="me", arg=Atom "yuki"},
                   TupleAttrRef {rel=Relation{name="grandfather", rid=0}, attr="him", arg=Var "X"}
                 ]
               )
