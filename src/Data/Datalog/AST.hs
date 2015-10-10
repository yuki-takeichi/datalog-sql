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
                    withClauses :: [CTE],
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

bridge :: String -> [ IndentedString ] -> [ IndentedString ] -> [ IndentedString ]
bridge str is1 (Line l2:is2) = case last is1 of
                                 Line l     -> init is1 ++ [ Line $ l ++ str ++ l2 ] ++ is2
                                 Indent _ _ -> is1      ++ [ Line $      str ++ l2 ] ++ is2
bridge str is1 is2           = case last is1 of
                                 Line l     -> init is1 ++ [ Line $ l ++ str ] ++ is2
                                 Indent _ _ -> is1      ++ [ Line        str ] ++ is2
                     

generateSQLCode :: SelectStmt -> IndentedString
generateSQLCode stmt = Indent 0 $ _generageSQLCode stmt ++ [Line ";"]

_generageSQLCode :: SelectStmt -> [ IndentedString ]
_generageSQLCode (UnionAll s1 s2) = _generageSQLCode s1 ++ [Line "union all"] ++ _generageSQLCode s2
_generageSQLCode (Union s1 s2) = _generageSQLCode s1 ++ [Line "union"] ++ _generageSQLCode s2
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
    cteSQL ((CTE _name stmt):[]) = [
                                    Line $ withLiteral ++ " " ++ _name ++ " as (",
                                    Indent 2 $ _generageSQLCode stmt,
                                    Line ")"
                                  ]
    cteSQL ((CTE _name stmt):cs) = [
                                      Line $ withLiteral ++ " " ++ _name ++ " as (",
                                      Indent 2 $ _generageSQLCode stmt,
                                      Line ")"
                                    ] ++ (concat (map notFirstCTE cs))
      where
        notFirstCTE :: CTE -> [ IndentedString ]
        notFirstCTE (CTE _name _stmt) = [
                                         Line $ ", " ++ _name ++ " as (",
                                         Indent 2 $ _generageSQLCode _stmt,
                                         Line ")"
                                      ]

    selectExprSQL :: [ SelectExpr ] -> [ IndentedString ]
    selectExprSQL [] = error "no column"
    selectExprSQL (s:[]) = [ appendToFirstLine "select " $ selectExpr s ]
    selectExprSQL (s:_ss) = [ appendToFirstLine "select " $ selectExpr s, Indent 5 $ map ((appendToFirstLine ", ") . selectExpr) _ss ]

    selectExpr :: SelectExpr -> IndentedString
    selectExpr (SelectExpr (ColumnRef _tableName _attrName) Nothing) = Line $ _tableName ++ "." ++ _attrName
    selectExpr (SelectExpr (ColumnRef _tableName _attrName) (Just _aliasName)) = Line $ _tableName ++ "." ++ _attrName ++ " as " ++ _aliasName
    selectExpr (SelectExpr (SqlStr str) Nothing) = Line $ "'" ++ str ++ "'"
    selectExpr (SelectExpr (SqlStr str) (Just _aliasName)) = Line $ "'" ++ str ++ "'" ++ " as " ++ _aliasName

    fromSQL :: [ TableRef ] -> [ IndentedString ]
    fromSQL [] = []
    fromSQL (t:[]) = [ appendToFirstLine "from " $ tableRef t ]
    fromSQL (t:_ts) = [ appendToFirstLine "from " $ tableRef t, Indent 3 $ map (\_t -> appendToFirstLine ", " $ tableRef _t) _ts]

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
    whereClauseSQL _ps = [
                           Line "where true",
                           Indent 2 $ concatMap (\x -> case predicateSQL x of
                                                    [] -> []
                                                    i:is -> appendToFirstLine ("and ") i:is) _ps
                         ]

    predicateSQL :: Predicate -> [ IndentedString ]
    predicateSQL (Equal e1 e2) = bridge " = " [exprSQL e1] [exprSQL e2]
    predicateSQL (NotEqual _ _) = undefined
    predicateSQL (And _ _) = undefined
    predicateSQL (Or _ _) = undefined
    predicateSQL (Not _ ) = undefined

    exprSQL :: Expr -> IndentedString
    exprSQL (ColumnRef _tableName _attrName) = Line $ _tableName ++ "." ++ _attrName
    exprSQL (SqlStr str)                     = Line $ "'" ++ str ++ "'" -- TOOD sanitize!!
