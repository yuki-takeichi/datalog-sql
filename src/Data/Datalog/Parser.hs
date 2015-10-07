{-# LANGUAGE FlexibleContexts #-}
module Data.Datalog.Parser ( 
  parse
) where

import Text.Parsec hiding (parse, sourceName)
import Control.Monad (void)
import qualified Control.Monad.State as S
import qualified Data.Map as M

import Data.Datalog.AST

type DatalogParser a = StatefulParser RelationCount a

type StatefulParser s a = ParsecT String () (S.State s) a
type RelationCount      = (M.Map String Integer)

parse :: String -> String -> Either ParseError DatalogStmt
parse sourceName code = S.evalState (runPT stmt () sourceName code) M.empty

stmt :: DatalogParser DatalogStmt
stmt = query <|> rule <|> fact

incrCounter :: String -> DatalogParser Integer
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

resetCounter :: DatalogParser ()
resetCounter = do S.put M.empty
                  return ()

fact :: DatalogParser DatalogStmt
fact = do r <- relation
          void $ char '.'
          return $ DatalogFact $ DatalogBody r

rule :: DatalogParser DatalogStmt
rule = do rs <- ruleOne `sepBy` char ';'
          void $ char '.'
          return $ DatalogRule rs

ruleOne :: DatalogParser (DatalogHead, DatalogBody)
ruleOne = do h <- headP
             void $ string ":-"
             b <- body
             return (h, b)

query :: DatalogParser DatalogStmt
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

queryHead :: DatalogParser DatalogHead
queryHead = do qrel <- queryHeadRelation
               return $ DatalogHead qrel

queryHeadRelation :: DatalogParser [TupleAttrRef]
queryHeadRelation = do void $ char '?'
                       --refs <- bracketed $ attrRef `sepBy` (char ',')
                       refs <- bracketed $ sepByComma attrRef
                       return $ map (\(at, ag) -> TupleAttrRef {rel=Relation{name="?", rid=0}, attr=at, arg=ag}) refs
                       -- TODO fact名をTupleAttrRefが持つのって冗長な(非正規的)な感じする
                       -- TODO ridが固定なのはやばい(が、queryがdisjinctiveじゃない現在は、
                       -- このままでいいかも。
                       

headP :: DatalogParser DatalogHead
headP = do r <- relation
           resetCounter
           return $ DatalogHead r

body :: DatalogParser DatalogBody
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

relation :: DatalogParser [TupleAttrRef]
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

attrRef :: DatalogParser (String, Arg)
attrRef = do attrName <- many alphaNum
             whitespace
             void $ char ':'
             whitespace
             a <- argP
             return (attrName, a)

argP :: DatalogParser Arg
argP = do a <- var <|> atom
          return $ a

atom :: DatalogParser Arg
atom = do atomName <- many alphaNum
          return $ Atom atomName

var :: DatalogParser Arg
var = do x <- upper
         xs <- many alphaNum
         return $ Var (x:xs)

whitespace :: ParsecT String u (S.State RelationCount) ()
whitespace = void $ many $ oneOf " \n\t"
