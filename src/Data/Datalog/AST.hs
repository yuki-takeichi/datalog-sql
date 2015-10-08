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
                    whereClause :: [Predicate] -- 連言
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
appendToFirstLine str (Indent n []) = Indent n [ Line str ]
appendToFirstLine str (Indent n (i:is)) = Indent n $ (appendToFirstLine str i):is

appendToLastLine :: String -> IndentedString -> IndentedString
appendToLastLine str (Line l) = Line $ l ++ str
appendToLastLine str (Indent n []) = Indent n [ Line str ]
appendToLastLine str (Indent n is) = Indent n $ (init is) ++ [appendToLastLine str (last is)]

bridge :: String -> IndentedString -> IndentedString -> IndentedString
bridge str (Line l) (Line l2) = Line $ l ++ str ++ l2


generateSQLCode :: SelectStmt -> IndentedString
generateSQLCode stmt = Indent 0 $ _generageSQLCode stmt ++ [Line ";"]

_generageSQLCode :: SelectStmt -> [ IndentedString ]
_generageSQLCode (UnionAll s1 s2) = [Line "(" ] ++ _generageSQLCode s1 ++ [Line ")", Line "union all", Line "("] ++ _generageSQLCode s2 ++ [Line ")"]
_generageSQLCode (Union s1 s2) = [Line "(" ] ++ _generageSQLCode s1 ++ [Line ")", Line "union", Line "("] ++ _generageSQLCode s2 ++ [Line ")"]
_generageSQLCode (Intersect s1 s2) = _generageSQLCode s1 ++ [Line "intersect"] ++ _generageSQLCode s2
_generageSQLCode (Except s1 s2) = _generageSQLCode s1 ++ [Line "except"] ++ _generageSQLCode s2
_generageSQLCode SelectStmt{withClauses=ctes,selectExprs=ss,fromTables=ts,whereClause=ps} = cteSQL ctes ++ selectExprSQL ss ++ fromSQL ts ++ whereClauseSQL ps
  where
    withLiteral = if any (\(CTE _ stmt) -> hasUnionAll stmt) ctes then "with recursive" else "with"
    hasUnionAll :: SelectStmt -> Bool
    hasUnionAll (SelectStmt{})    = False
    hasUnionAll (UnionAll _ _)    = True
    hasUnionAll (Union s1 s2)     = hasUnionAll s1 || hasUnionAll s2
    hasUnionAll (Intersect s1 s2) = hasUnionAll s1 || hasUnionAll s2
    hasUnionAll (Except s1 s2)    = hasUnionAll s1 || hasUnionAll s2

    cteSQL :: [ CTE ] -> [ IndentedString ]
    cteSQL [] = []
    cteSQL ((CTE name stmt):[]) = [
                                     Line $ withLiteral ++ " " ++ name ++ " as (",
                                     Indent 2 $ _generageSQLCode stmt,
                                     Line ")"
                                   ]
    cteSQL ((CTE name stmt):ctes) = [
                                      Line $ withLiteral ++ " " ++ name ++ " as (",
                                      Indent 2 $ _generageSQLCode stmt,
                                      Line ")"
                                    ] ++ (concat (map notFirstCTE ctes))
      where
        notFirstCTE :: CTE -> [ IndentedString ]
        notFirstCTE (CTE name stmt) = [
                                         Line $ ", " ++ name ++ " as (",
                                         Indent 2 $ _generageSQLCode stmt,
                                         Line ")"
                                      ]

    selectExprSQL :: [ SelectExpr ] -> [ IndentedString ]
    selectExprSQL [] = error "no column"
    selectExprSQL (s:[]) = [ appendToFirstLine "select " $ selectExpr s ]
    selectExprSQL (s:ss) = [ appendToFirstLine "select " $ selectExpr s, Indent 5 $ map ((appendToFirstLine ", ") . selectExpr) ss ]

    selectExpr :: SelectExpr -> IndentedString
    selectExpr (SelectExpr (ColumnRef _tableName _attrName) Nothing) = Line $ _tableName ++ "." ++ _attrName
    selectExpr (SelectExpr (ColumnRef _tableName _attrName) (Just _aliasName)) = Line $ _tableName ++ "." ++ _attrName ++ " as " ++ _aliasName
    selectExpr (SelectExpr (SqlStr str) Nothing) = Line $ "\"" ++ str ++ "\""
    selectExpr (SelectExpr (SqlStr str) (Just _aliasName)) = Line $ "\"" ++ str ++ "\"" ++ " as " ++ _aliasName

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

    whereClauseSQL :: [ Predicate ] -> [ IndentedString ]
    whereClauseSQL [] = []
    whereClauseSQL ps = [
                          Line "where true",
                          Indent 2 $ map (appendToFirstLine ("and ") . predicateSQL) ps
                        ]

    predicateSQL :: Predicate -> IndentedString
    predicateSQL (Equal e1 e2) = bridge " = " (exprSQL e1) (exprSQL e2)
    predicateSQL (NotEqual _ _) = undefined
    predicateSQL (And _ _) = undefined
    predicateSQL (Or _ _) = undefined
    predicateSQL (Not _ ) = undefined

    exprSQL :: Expr -> IndentedString
    exprSQL (ColumnRef _tableName _attrName) = Line $ _tableName ++ "." ++ _attrName
    exprSQL (SqlStr str)                     = Line $ "\"" ++ str ++ "\"" -- TOOD sanitize!!
{-
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
