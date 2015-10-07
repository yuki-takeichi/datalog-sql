{-# LANGUAGE FlexibleContexts #-}
module Data.Datalog.Parser ( 
  parse
, fact
, rule
, query
) where

import Text.Parsec
import Control.Monad (void)
import qualified Control.Monad.State as S
import qualified Data.Map as M

import Data.Datalog.AST

type StatefulParser s a = ParsecT String () (S.State s) a
type RelationCount      = (M.Map String Integer)

incrCounter :: String -> StatefulParser RelationCount Integer
incrCounter key = do s <- S.get
                     let rId = getId key s 
                     S.modify $ incr key
                     return $ rId
  where
    incr k m = case M.lookup k m of
                 Just i  -> M.insert k (i+1) m
                 Nothing -> M.insert k 1 m
    getId :: String -> M.Map String Integer -> Integer
    getId k m = case M.lookup k m of
                  Just v -> v
                  Nothing -> 0

resetCounter :: StatefulParser RelationCount ()
resetCounter = do S.put M.empty
                  return ()

fact :: StatefulParser RelationCount DatalogFact
fact = do r <- relation
          void $ char '.'
          return $ DatalogFact $ DatalogBody r

rule :: StatefulParser RelationCount DatalogRule
rule = do rs <- ruleOne `sepBy` char ';'
          void $ char '.'
          return $ DatalogRule rs

ruleOne :: StatefulParser RelationCount (DatalogHead, DatalogBody)
ruleOne = do h <- headP
             void $ string ":-"
             b <- body
             return (h, b)

query :: StatefulParser RelationCount DatalogQuery
query = do whitespace
           h <- queryHead
           whitespace
           void $ string ":-"
           whitespace
           b <- body
           whitespace
           void $ char '.'
           whitespace
           return $ DatalogQuery h b -- TODO ?であることの制約

queryHead :: StatefulParser RelationCount DatalogHead
queryHead = do qrel <- queryHeadRelation
               return $ DatalogHead qrel

queryHeadRelation :: StatefulParser RelationCount [TupleAttrRef]
queryHeadRelation = do void $ char '?'
                       --refs <- bracketed $ attrRef `sepBy` (char ',')
                       refs <- bracketed $ sepByComma attrRef
                       return $ map (\(at, ag) -> TupleAttrRef {rel=Relation{name="?", rid=0}, attr=at, arg=ag}) refs
                       -- TODO fact名をTupleAttrRefが持つのって冗長な(非正規的)な感じする
                       -- TODO ridが固定なのはやばい(が、queryがdisjinctiveじゃない現在は、
                       -- このままでいいかも。
                       

headP :: StatefulParser RelationCount DatalogHead
headP = do r <- relation
           resetCounter
           return $ DatalogHead r

body :: StatefulParser RelationCount DatalogBody
body = do rels <- sepByComma1 $ relation
          resetCounter
          return $ DatalogBody $ concat rels

sepByComma :: Stream s m Char => ParsecT s u m a -> ParsecT s u m [a]
sepByComma p = do v <- p `sepBy` char ','
                  return v

sepByComma1 :: ParsecT String u (S.State RelationCount) a
               -> ParsecT String u (S.State RelationCount) [a]
sepByComma1 p = do v <- p `sepBy1` (whitespace >> char ',' >> whitespace)
                   return v

relation :: StatefulParser RelationCount [TupleAttrRef]
relation = do relName <- many alphaNum
              refs <- bracketed $ sepByComma attrRef
              rId <- incrCounter relName
              let rels = map (\(at, ag) -> TupleAttrRef {rel=Relation{name=relName, rid=rId}, attr=at, arg=ag}) refs
              return rels

bracketed :: ParsecT String u (S.State RelationCount) b
             -> ParsecT String u (S.State RelationCount) b
bracketed a = do void $ char '('
                 whitespace
                 v <- a
                 whitespace
                 void $ char ')'
                 return v

attrRef :: StatefulParser RelationCount (String, Arg)
attrRef = do attrName <- many alphaNum
             whitespace
             void $ char ':'
             whitespace
             a <- argP
             return (attrName, a)

argP :: StatefulParser RelationCount Arg
argP = do a <- var <|> atom
          return $ a

atom :: StatefulParser RelationCount Arg
atom = do atomName <- many alphaNum
          return $ Atom atomName

var :: StatefulParser RelationCount Arg
var = do x <- upper
         xs <- many alphaNum
         return $ Var (x:xs)

whitespace :: ParsecT String u (S.State RelationCount) ()
whitespace = void $ many $ oneOf " \n\t"
