{-
 - grandparent(me: X, him: Y) :- parent(me: X, him: Z), parent(me: Z, him: Y).
 -
 - ancestor(me: X, him: Y) :- parent(me: X, him: Y).
 - ancestor(me: X, him: Y) :- parent(me: X, him: Z), ancestor(me: Z, him: Y).
 -
 - ?(him: X) :- ancestor(me: yuki, him: X), grandparent(me: masaki, him: X).
 -}

{-
 - TODO
 - * genSQLASTが意図通り動くか検証 (テストを書くのもあり)
 - * with recursiveの判定
 -}

module Data.Datalog.AST ( 
  TupleAttrRef(..)
, Arg(..)
, SelectExpr(..)
, Expr(..)
, TableRef(..)
, SelectStmt(..)
, Predicate(..)
, DatalogHead(..)
, DatalogBody(..)
, genSQLAST
, genSQLString
, IndentedString(..)
, indent
, rule1
, rule2
, joins
) where

import Prelude hiding (lookup, head)
import Data.List (groupBy, nub)
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

data TupleAttrRef = TupleAttrRef {
  fact :: String, -- TODO naming
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
                | Except SelectStmt SelectStmt deriving (Show, Eq)
data CTE = CTE String SelectStmt deriving (Show, Eq)
data SelectExpr = SelectExpr Expr (Maybe String) deriving (Show, Eq)
data TableRef = Table String
              | SubSelect SelectStmt String deriving (Show, Eq)
data Predicate = Equal Expr Expr deriving (Show, Eq)
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
genSQLAST _ _ = undefined


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

    freeVarNames :: [String]
    freeVarNames = nub $ catMaybes $ map varName $ head

    freeVars :: [ TupleAttrRef ]
    freeVars = filter isVarRef head

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
    _selectExprs = catMaybes $ map ((fmap (\(h, r) -> SelectExpr (ColumnRef (fact r) (attr r)) (Just $ attr h))).hage) head
      where
        hage r@TupleAttrRef{arg=Var vname} = Just (r, (varAttrs vname body)!!0) -- !!0は重複ロジックなのでリファクタ必要
        hage _ = Nothing

    _fromTables :: [ TableRef ]
    _fromTables = map Table $ nub $ map fact body -- TODO parent1 parent2問題を解決する

    _withClauses :: [ CTE ]
    _withClauses = map (uncurry CTE) $ catMaybes $ map findRules $ catMaybes $ map (\x -> case x of
                                                                        Table name -> Just name
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
        joinEquals (r1@TupleAttrRef{arg=Var _}, r2@TupleAttrRef{arg=Var _}) = Just $ Equal (ColumnRef (fact r1) (attr r1))
                                                                                     (ColumnRef (fact r2) (attr r2))
        joinEquals _ = Nothing

        constraintEquals :: TupleAttrRef -> Maybe Predicate
        constraintEquals r@TupleAttrRef{arg=Atom aname} = Just $ Equal (ColumnRef (fact r) (attr r))
                                                          (SqlStr (aname))
        constraintEquals _ = Nothing

foo :: String -> TupleAttrRef -> Bool
foo targetVarName TupleAttrRef { arg=Var varName } = targetVarName == varName
foo _ _ = False


-- grandparent(me: X, him: Y) :- parent(me: X, him: Z), parent(me: Z, him: Y).
rule1 :: DatalogRule
rule1 = DatalogRule [(DatalogHead [
                 TupleAttrRef {fact="?", attr="me", arg=Var "X"},
                 TupleAttrRef {fact="?", attr="him", arg=Var "Y"}
               ],
               DatalogBody [ 
                 TupleAttrRef {fact="parent1", attr="me", arg=Var "X"}, -- TODO 述語の出現ごとに連番を降る
                 TupleAttrRef {fact="parent1", attr="him", arg=Var "Z"},
                 TupleAttrRef {fact="parent2", attr="me", arg=Var "Z"},
                 TupleAttrRef {fact="parent2", attr="him", arg=Var "Y"}
               ])]
{-
 - with grandparent as (
 -   select parent1.me as me
 -        , parent2.him as him
 -   from parent as parent1
 -      , parent as parent2
 -   where parent1.him = parent2.me
 - )
 -}

-- ancestor(me: X, him: Y) :- parent(me: X, him: Y).
-- ancestor(me: X, him: Y) :- parent(me: X, him: Z), ancestor(me: Z, him: Y).
rule2 :: DatalogRule
rule2 = DatalogRule [(DatalogHead [
                 TupleAttrRef {fact="ancestor", attr="me", arg=Var "X"},
                 TupleAttrRef {fact="ancestor", attr="him", arg=Var "Y"}
               ],
               DatalogBody [ 
                 TupleAttrRef {fact="parent", attr="me", arg=Var "X"},
                 TupleAttrRef {fact="parent", attr="him", arg=Var "Y"}
               ]),
              (DatalogHead [
                 TupleAttrRef {fact="ancestor", attr="me", arg=Var "X"},
                 TupleAttrRef {fact="ancestor", attr="him", arg=Var "Y"}
               ],
               DatalogBody [
                 TupleAttrRef {fact="parent", attr="me", arg=Var "X"},
                 TupleAttrRef {fact="parent", attr="him", arg=Var "Z"},
                 TupleAttrRef {fact="ancestor", attr="me", arg=Var "Z"},
                 TupleAttrRef {fact="ancestor", attr="him", arg=Var "Y"}
               ])]
{- with recursive ancestor as (
 -   select parent.me as me
 -        , parent.him as him
 -   from parent
 -   union all
 -   select parent.me as me
 -        , ancestor.him as him
 -   from parent
 -      , ancestor
 -   where parent.him = ancestor.me
 - )
 -}




joins :: [TupleAttrRef] -> [[TupleAttrRef]]
joins refs = let varRefs = filter isVarRef refs
                    in groupBy (\r1 r2 -> arg r1 == arg r2) varRefs

isVarRef :: TupleAttrRef -> Bool
isVarRef TupleAttrRef{arg=Var _} = True
isVarRef _ = False

something :: [a] -> [(a,a)] -- TODO rename
something [] = []
something (x:[]) = [] -- 変数への参照がひとつしかない場合は空
something (x:xs) = reverse $ somethingHoge x xs []
  where somethingHoge _ [] piyo = piyo
        somethingHoge y (y':ys) piyo = somethingHoge y ys $ (y, y') : piyo

genSQLString :: SelectStmt -> IndentedString
genSQLString stmt = genSQLStringIndented stmt

genSQLStringIndented :: SelectStmt -> IndentedString
genSQLStringIndented SelectStmt{withClauses=_withClause,selectExprs=_selectExprs,fromTables=_fromTables,whereClause=_whereClause} = undefined

tableRefString :: TableRef -> IndentedString
tableRefString (Table tableName) = Block [tableName]
tableRefString (SubSelect stmt alias) = Indent 0 [
                                                   Block ["("],
                                                   Indent 2 [genSQLStringIndented stmt],
                                                   Block [")" ++ " as " ++ alias]
                                                 ]

whereClauseString :: [Predicate] -> IndentedString
whereClauseString preds = undefined

predicateString :: Predicate -> String
predicateString (Equal lexpr rexpr) = exprString lexpr ++ " = " ++ exprString rexpr

selectExprString :: SelectExpr -> String
selectExprString (SelectExpr expr (Just asName)) = exprString expr ++ " as " ++ asName
selectExprString (SelectExpr expr (Nothing))     = exprString expr

exprString :: Expr -> String
exprString (ColumnRef table column) = table ++ "." ++ column
exprString (SqlStr str) = "\"" ++ str ++ "\""

