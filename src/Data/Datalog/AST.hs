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
, DatalogFact(..)
, DatalogRule(..)
, DatalogQuery(..)
, CTE(..)
, Relation(..)
, genSQLAST
--, genSQLString
--, genSQLStringIndented
, IndentedString(..)
, indent
--, joins
) where

import Prelude hiding (lookup, head)
import Data.List ({-groupBy, -}nub)
import Data.Maybe (catMaybes, isNothing)
import Data.Map (Map, lookup)
--import Data.List.NonEmpty ( NonEmpty(..) )

data DatalogStmt = DatalogStmtFact DatalogFact
                 | DatalogStmtRule DatalogRule
                 | DatalogStmtQuery DatalogQuery
                 deriving (Show, Eq)
newtype DatalogFact  = DatalogFact DatalogBody deriving (Show, Eq)
newtype DatalogRule  = DatalogRule [ (DatalogHead, DatalogBody) ] deriving (Show, Eq)
data    DatalogQuery = DatalogQuery DatalogHead DatalogBody deriving (Show, Eq)

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
                    withClauses :: [CTE], -- union allのいるSelectStmtが一つでもあればRECURSIVEつける
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
                    | Block [String] deriving (Show, Eq)

indent :: IndentedString -> String
indent is = unlines $ padding 0 is
  where
    padding :: Int -> IndentedString -> [String]
    padding level (Block strs) = shift level strs
    padding level (Indent innerLevel bs) = concatMap (padding $ level+innerLevel) bs

    shift :: Int -> [String] -> [String]
    shift level = map ((take level $ cycle " ") ++)


genSQLAST :: Map String DatalogRule
             -> [(DatalogHead, DatalogBody)]
             -> SelectStmt
genSQLAST rules rulesConverted = flattenWithClauses $ foldl1 Union $ map (genSimpleSelectAST rules) rulesConverted
  where
    flattenWithClauses :: SelectStmt -> SelectStmt
    flattenWithClauses = id -- TODO これを入れると読みやすいっていう人と読みにくいっていう人の2パターンいそう。optionalにできるとよい。


genSimpleSelectAST :: Map String DatalogRule -> (DatalogHead, DatalogBody) -> SelectStmt
genSimpleSelectAST rules (DatalogHead head, DatalogBody body) = SelectStmt {
  withClauses = _withClauses,
  selectExprs = _selectExprs,
  fromTables = _fromTables,
  whereClause = _whereClause
}
  where 
    varNames :: [String]
    varNames = nub $ catMaybes $ map varName $ head++body

    --freeVarNames :: [String]
    --freeVarNames = nub $ catMaybes $ map varName $ head

    --freeVars :: [ TupleAttrRef ]
    --freeVars = filter isVarRef head

    varName :: TupleAttrRef -> Maybe String
    varName TupleAttrRef { arg=Var vname } = Just vname
    varName _ = Nothing

    varAttrs :: String -> [TupleAttrRef] -> [TupleAttrRef]
    varAttrs vname = filter (\x -> case varName x of
                                     Just y -> y == vname
                                     Nothing -> False)

    joins :: [[ TupleAttrRef ]]
    joins = map (\vname -> varAttrs vname body) varNames -- TODO groupBy を使ってもっと綺麗に書きたいよー
    -- TODO ?が付いているfactは除外しないと...

    constraints :: [TupleAttrRef]
    constraints = filter (isNothing.varName) body

    _selectExprs :: [ SelectExpr ]
    _selectExprs = catMaybes $ map ((fmap (\(h, r) -> SelectExpr (ColumnRef (tableName $ rel r) (attr r)) (Just $ attr h))).hage) head
      where
        hage r@TupleAttrRef{arg=Var vname} = Just (r, (varAttrs vname body)!!0) -- !!0は重複ロジックなのでリファクタ必要
        hage _ = Nothing

    _fromTables :: [ TableRef ]
    _fromTables = map (\x -> (Table x Nothing)) $ nub $ map (tableName . rel) body -- TODO parent1 parent2問題を解決する

    _withClauses :: [ CTE ]
    _withClauses = map (uncurry CTE) $ catMaybes $ map findRules $ catMaybes $ map (\x -> case x of
                                                                        Table name_ _ -> Just name_ -- TODO handle Table alias
                                                                        _ -> Nothing) _fromTables
      where
        findRules :: String -> Maybe (String, SelectStmt)
        findRules rname = (fmap (\x -> (rname, hogehoge x))) . (\x -> lookup x rules) $ rname
          where
            hogehoge :: DatalogRule -> SelectStmt
            hogehoge = undefined


    _whereClause :: [ Predicate ]
    _whereClause = catMaybes $ map joinEquals (concat $ map something joins) ++ map constraintEquals constraints
      where
        joinEquals :: (TupleAttrRef, TupleAttrRef) -> Maybe Predicate
        joinEquals (r1@TupleAttrRef{arg=Var _}, r2@TupleAttrRef{arg=Var _}) = Just $ Equal (ColumnRef (tableName $ rel r1) (attr r1))
                                                                                     (ColumnRef (tableName $ rel r2) (attr r2))
        joinEquals _ = Nothing

        constraintEquals :: TupleAttrRef -> Maybe Predicate
        constraintEquals r@TupleAttrRef{arg=Atom aname} = Just $ Equal (ColumnRef (tableName $ rel r) (attr r))
                                                          (SqlStr (aname))
        constraintEquals _ = Nothing

tableName :: Relation -> String
tableName r = (name r) ++ (show $ rid r)

--joins :: [TupleAttrRef] -> [[TupleAttrRef]]
--joins refs = let varRefs = filter isVarRef refs
                    --in groupBy (\r1 r2 -> arg r1 == arg r2) varRefs

--isVarRef :: TupleAttrRef -> Bool
--isVarRef TupleAttrRef{arg=Var _} = True
--isVarRef _ = False

something :: [a] -> [(a,a)] -- TODO rename
something [] = []
something (_:[]) = [] -- 変数への参照がひとつしかない場合は空
something (x:xs) = reverse $ somethingHoge x xs []
  where somethingHoge _ [] piyo = piyo
        somethingHoge y (y':ys) piyo = somethingHoge y ys $ (y, y') : piyo

{-
genSQLString :: SelectStmt -> IndentedString
genSQLString stmt = genSQLStringIndented stmt

genSQLStringIndented :: SelectStmt -> IndentedString
genSQLStringIndented SelectStmt{withClauses=_withClause,selectExprs=_selectExprs,fromTables=_fromTables,whereClause=_whereClause} = undefined
  where
    tableRef :: TableRef -> IndentedString
    tableRef (Table tableName (Just aliasName)) = Block [tableName ++ " as " ++ aliasName]
    tableRef (Table tableName Nothing) = Block [tableName]
    tableRef (SubSelect stmt alias) = Indent 0 [
                                                 Block ["("],
                                                 Indent 2 [genSQLStringIndented stmt],
                                                 Block [")" ++ " as " ++ alias]
                                               ]
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
