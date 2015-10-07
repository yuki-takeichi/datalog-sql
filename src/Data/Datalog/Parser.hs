module Data.Datalog.Parser ( 
  fact
, rule
, query
) where

import Text.Parsec
import Text.Parsec.String
import Control.Monad (void)

import Data.Datalog.AST

fact :: Parser DatalogFact
fact = do r <- relation
          void $ char '.'
          return $ DatalogFact $ DatalogBody r

rule :: Parser DatalogRule
rule = do h <- headP
          b <- body
          return $ DatalogRule [(h, b)] -- TODO 選言への対応(cabalのDatalogを参考にする)

query :: Parser DatalogQuery
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

queryHead :: Parser DatalogHead
queryHead = do qrel <- queryHeadRelation
               return $ DatalogHead qrel

queryHeadRelation :: Parser [TupleAttrRef]
queryHeadRelation = do void $ char '?'
                       --refs <- bracketed $ attrRef `sepBy` (char ',')
                       refs <- bracketed $ sepByComma attrRef
                       return $ map (\(at, ag) -> TupleAttrRef {rel=Relation{name="?", rid=0}, attr=at, arg=ag}) refs
                       -- TODO fact名をTupleAttrRefが持つのって冗長な(非正規的)な感じする
                       -- TODO ridが固定なのはやばい
                       

headP :: Parser DatalogHead
headP = do r <- relation
           return $ DatalogHead r

body :: Parser DatalogBody
body = do rels <- sepByComma1 $ relation
          return $ DatalogBody $ concat rels

sepByComma :: Parser a -> Parser [a]
sepByComma p = do v <- p `sepBy` char ',' --(whitespace >> char ',' >> whitespace)
                  return v

sepByComma1 :: Parser a -> Parser [a]
sepByComma1 p = do v <- p `sepBy1` (whitespace >> char ',' >> whitespace)
                   return v

relation :: Parser [TupleAttrRef]
relation = do relName <- many alphaNum
              refs <- bracketed $ sepByComma attrRef
              return $ map (\(at, ag) -> TupleAttrRef {rel=Relation{name=relName, rid=0}, attr=at, arg=ag}) refs
              -- TODO fact名をTupleAttrRefが持つのって冗長な(非正規的)な感じする
              -- TODO fix rid

bracketed :: Parser a -> Parser a
bracketed a = do void $ char '('
                 whitespace
                 v <- a
                 whitespace
                 void $ char ')'
                 return v

attrRef :: Parser (String, Arg)
attrRef = do attrName <- many alphaNum
             whitespace
             void $ char ':'
             whitespace
             a <- argP
             return (attrName, a)

argP :: Parser Arg
argP = do a <- var <|> atom
          return $ a

atom :: Parser Arg
atom = do atomName <- many alphaNum
          return $ Atom atomName

var :: Parser Arg
var = do x <- upper
         xs <- many alphaNum
         return $ Var (x:xs)

whitespace :: Parser ()
whitespace = void $ many $ oneOf " \n\t"
