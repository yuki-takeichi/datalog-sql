module Data.Datalog.AST ( 
  TupleAttrRef(..)
, Arg(..)
, SelectExpr(..)
, Expr(..)
, TableRef(..)
, SelectStmt(..)
, Predicate(..)
, DatalogStmt(..)
, DatalogHead(..)
, DatalogBody(..)
, CTE(..)
, Relation(..)
, generateSQLCode
, IndentedString(..)
, indent
--, joins
) where

import Prelude hiding (lookup, head)
--import Data.List.NonEmpty ( NonEmpty(..) )

data DatalogStmt = DatalogFact DatalogBody
                 | DatalogRule [ (DatalogHead, DatalogBody) ]
                 | DatalogQuery DatalogHead DatalogBody
                 deriving (Show, Eq)

newtype DatalogHead = DatalogHead [TupleAttrRef] deriving (Show, Eq)
newtype DatalogBody = DatalogBody [TupleAttrRef] deriving (Show, Eq)

data Relation = Relation { name :: String, rid :: Integer } deriving (Show, Eq)

data TupleAttrRef = TupleAttrRef {
  rel :: Relation,
  attr :: String,
  arg :: Arg -- TODO naming
} deriving (Show, Eq)

data Arg = Atom String
         | Var String
         deriving (Show, Eq)

-- SQL.AST
data SelectStmt = SelectStmt { -- order by, limitいれるとしたらこと
                    withClauses :: [CTE], -- union allのいるSelectStmtが1つでもあればRECURSIVEつける
                    selectExprs :: [SelectExpr],
                    fromTables  :: [TableRef],
                    whereClause :: [Predicate]
                  }
                | Union SelectStmt SelectStmt
                | UnionAll SelectStmt SelectStmt
                | Intersect SelectStmt SelectStmt
                | Except SelectStmt SelectStmt
                deriving (Show, Eq)
data CTE = CTE String SelectStmt deriving (Show, Eq)
data SelectExpr = SelectExpr Expr (Maybe String) deriving (Show, Eq)
data TableRef = Table String (Maybe String)
              | SubSelect SelectStmt String deriving (Show, Eq)
data Predicate = Equal    Expr Expr
               | NotEqual Expr Expr
               | And      Expr Expr
               | Or       Expr Expr
               | Not      Expr 
               deriving (Show, Eq)
data Expr = ColumnRef String String
          | SqlStr String deriving (Show, Eq)

data IndentedString = Indent Int [IndentedString]
                    | Line String deriving (Show, Eq)

indent :: IndentedString -> String
indent is = unlines $ padding 0 is
  where
    padding :: Int -> IndentedString -> [String]
    padding level (Line str) = [shift level str]
    padding level (Indent innerLevel bs) = concatMap (padding $ level+innerLevel) bs

    shift :: Int -> String -> String
    shift level = ((take level $ cycle " ") ++)

appendToFirstLine :: String -> IndentedString -> IndentedString
appendToFirstLine str (Line l) = Line $ str ++ l
appendToFirstLine str (Indent n []) = Indent n []
appendToFirstLine str (Indent n (i:is)) = Indent n $ (appendToFirstLine str i):is


generateSQLCode :: SelectStmt -> IndentedString
generateSQLCode stmt = _generageSQLCode stmt

_generageSQLCode :: SelectStmt -> IndentedString
_generageSQLCode SelectStmt{withClauses=ctes,selectExprs=ss,fromTables=ts,whereClause=ps} = Indent 0 $ selectExprSQL ss ++ fromSQL ts ++ [ Line ";" ]
  where
    selectExprSQL :: [ SelectExpr ] -> [ IndentedString ]
    selectExprSQL (s:[]) = [ appendToFirstLine "select " $ selectExpr s ]
    selectExprSQL (s:ss) = [ appendToFirstLine "select " $ selectExpr s, Indent 5 $ map ((appendToFirstLine ", ") . selectExpr) ss ]

    selectExpr :: SelectExpr -> IndentedString
    selectExpr (SelectExpr (ColumnRef _tableName _attrName) Nothing) = Line $ _tableName ++ "." ++ _attrName
    selectExpr (SelectExpr (ColumnRef _tableName _attrName) (Just _aliasName)) = Line $ _tableName ++ "." ++ _attrName ++ " as " ++ _aliasName

    fromSQL :: [ TableRef ] -> [ IndentedString ]
    fromSQL [] = []
    fromSQL (t:[]) = [ appendToFirstLine "from " $ tableRef t ]
    fromSQL (t:ts) = [ appendToFirstLine "from " $ tableRef t, Indent 3 $ map (\t -> appendToFirstLine ", " $ tableRef t) ts]

    tableRef :: TableRef -> IndentedString
    tableRef (Table tableName (Just aliasName)) = Line $ tableName ++ " as " ++ aliasName
    tableRef (Table tableName Nothing) = Line tableName
    tableRef (SubSelect stmt alias) = Indent 0 [
                                                 Line "(",
                                                 Indent 2 [generateSQLCode stmt],
                                                 Line $ ")" ++ " as " ++ alias
                                               ]
{-
    whereClause :: Predicate -> IndentedString
    whereClause expr = Indent 0 [
                          Block ["where true"],
                          Indent 2 [
                            predicate expr
                          ]
                        ]

    predicate :: Predicate -> IndentedString
    predicate (Equal e1 e2) = binaryOperator "=" e1 e2
    predicate (NotEqual e1 e2) = binaryOperator "!=" e1 e2
    predicate (And es) = polyadicOperator "and" "true" es
    predicate (Or es) = polyadicOperator "or" "false" es
    predicate (Not e1) = unaryOperator "not" e1

    unaryOperator :: String -> Predicate -> IndentedString
    unaryOperator op expr = indentWithStr (" "++op++" ") $ predicate expr

    indentWithStr :: String -> IndentedString -> IndentedString
    indentWithStr str (Block [])        = Block []
    indentWithStr str (Block (l:ls))    = Indent 0 $ Block [str++l]:[Indent (length str) [Block ls]]
    indentWithStr str (Indent n [])     = Indent n []
    indentWithStr str (Indent n (i:is)) = Indent n (indentWithStr str i:is)

    binaryOperator :: String -> Predicate -> Predicate -> IndentedString
    binaryOperator op e1 e2 = case (predicate e1, predicate e2) of
                                (Block [], Block []) -> Block []
                                (Block l1, Block []) -> Block []
                                (Block [], Block l2) -> Block []
                                (Block (l1:[]), Block (l2:[])) -> Block $ [l1++" "++op++" "++l2]
                                (Block (l1:ls1), Block (l2:ls2)) -> undefined
                                (Indent is, Block (l2:ls2)) -> undefined
                                (Block (l1:ls1), Indent is) -> undefined
                                (Indent is1, Indent is2) -> undefined

    -- parens are optional
    polyadicOperator :: String -> String -> [Predicate] -> IndentedString
    polyadicOperator op identityElement es = indentWithStr ("( "++identityElement++" "++op) $ Indent 0 $ map predicate es  -- TODO かっこの扱いを考える
-}
