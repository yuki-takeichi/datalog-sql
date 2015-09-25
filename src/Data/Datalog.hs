module Data.Datalog (
  fact,
  facts,
  fromSource,
  query,
  Facts
) where

import Data.Set as S hiding (filter, map)
import Text.Parsec
import Text.Parsec.String
import Control.Applicative ((<$>), (<*>))

data Fact = Fact String (Atom, Atom) deriving (Show, Ord, Eq)
data Atom = Atom String deriving (Show, Ord, Eq)
data Variable = Variable String deriving (Show, Ord, Eq)
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
