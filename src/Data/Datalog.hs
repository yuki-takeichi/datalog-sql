module Data.Datalog (
  fact,
  facts,
  fromSource,
  query,
  eval,
  Facts
) where

import Data.Map as M hiding (filter, map)
import Data.Set as S hiding (filter, map)
import Text.Parsec
import Text.Parsec.String
import Control.Applicative ((<$>), (<*>))

data Fact = Fact String (Atom, Atom) deriving (Show, Ord, Eq)
data Atom = Atom String deriving (Show, Ord, Eq)
data Variable = Variable String deriving (Show, Ord, Eq)
data Result = Result0 Bool
            | Result1 (Map Variable [Atom])
            | Result2 (Map (Variable, Variable) [(Atom, Atom)])
            deriving Show
data Query = Query String (Xor Atom Variable, Xor Atom Variable) deriving Show
type Facts = Set Fact
data Xor a b = A a | B b deriving Show

facts :: Parser Facts
facts = S.fromList <$> fact `sepEndBy` spaces

fact :: Parser Fact
fact = do predicate <- many alphaNum
          _ <- char '('
          arg1 <- atom
          _ <- char ','
          _ <- many (char ' ')
          arg2 <- atom
          _ <- char ')'
          _ <- char '.'
          return $ Fact predicate (arg1, arg2)

atom :: Parser Atom
atom = Atom <$> ( (:) <$> lower <*> many alphaNum )

fromSource :: String -> IO (Either ParseError Facts)
fromSource path = parseFromFile facts path

query :: Parser Query
query = do predicate <- many alphaNum
           _ <- char '('
           arg1 <- queryArg
           _ <- char ','
           _ <- many (char ' ')
           arg2 <- queryArg
           _ <- char ')'
           _ <- char '.'
           return $ Query predicate (arg1, arg2)

queryArg :: Parser (Xor Atom Variable)
queryArg =  A <$> atom <|> B <$> variable

variable :: Parser Variable
variable = Variable <$> ( (:) <$> upper <*> many alphaNum )

eval :: Facts -> Query -> Result
eval fs (Query prd (A atom1, A atom2)) = eval0  fs prd (atom1, atom2)
eval fs (Query prd (A atom_, B var))   = eval1l fs prd (atom_, var)
eval fs (Query prd (B var,   A atom_)) = eval1r fs prd (var,   atom_)
eval fs (Query prd (B var1,  B var2))  = eval2  fs prd (var1,  var2)

eval0 :: Facts -> String -> (Atom, Atom) -> Result
eval0 fs qpred qtpl = Result0 $ any (\(Fact prd tpl) -> qpred == prd && qtpl == tpl) $ S.toList fs

eval1l :: Facts -> String -> (Atom, Variable) -> Result
eval1l fs qprd (qatom, qvar) = let values = map (\(Fact _ tpl) -> snd tpl) $ filter (\(Fact prd tpl) -> qprd == prd && fst tpl == qatom) $ S.toList fs
                               in Result1 $ M.fromList [(qvar, values)]

eval1r :: Facts -> String -> (Variable, Atom) -> Result
eval1r fs qprd (qvar, qatom) = let values = map (\(Fact _ tpl) -> fst tpl) $ filter (\(Fact prd tpl) -> qprd == prd && snd tpl == qatom) $ S.toList fs
                               in Result1 $ M.fromList [(qvar, values)]

eval2 :: Facts -> String -> (Variable, Variable) -> Result
eval2 fs qprd qvtpl = let tpls = map (\(Fact _ tpl) -> tpl) $ filter (\(Fact prd _) -> qprd == prd) $ S.toList fs
                      in Result2 $ M.fromList [(qvtpl, tpls)]
