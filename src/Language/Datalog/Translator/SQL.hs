module Language.Datalog.Translator.SQL (
  genSQLAST
) where

import Prelude hiding (lookup, head)
import Data.List ({-groupBy, -}nub)
import Data.Maybe (catMaybes, isNothing)
import Data.Map (Map, lookup)

import Data.Datalog.AST

type VarName = String

genSQLAST :: Map String DatalogStmt -- TODO MapのvalueがDatalotStmtだとlookupで毎回分岐の実装を強いられるのが辛いので変更する。
             -> DatalogStmt
             -> SelectStmt
genSQLAST rules stmt = flattenWithClauses $ foldl1 Union $ [ genSimpleSelectAST rules stmt ]
  where
    flattenWithClauses :: SelectStmt -> SelectStmt
    flattenWithClauses = id -- TODO これを入れると読みやすいっていう人と読みにくいっていう人の2パターンいそう。optionalにできるとよい。

varNames :: [TupleAttrRef] -> [TupleAttrRef] -> [String]
varNames head body = nub $ catMaybes $ map varName $ head++body

varName :: TupleAttrRef -> Maybe VarName
varName TupleAttrRef{ arg=Var vname } = Just vname
varName _                             = Nothing

varAttrs :: VarName -> [TupleAttrRef] -> [TupleAttrRef]
varAttrs vname = filter (\x -> case varName x of
                                 Just y -> y == vname
                                 Nothing -> False)

_fromTables :: [ TupleAttrRef ] -> [ TableRef ]
_fromTables body = nub $ map (tableRef . rel) body
  where
    tableRef :: Relation -> TableRef
    tableRef Relation{name=rname, rid=0}   = Table rname Nothing
    tableRef Relation{name=rname, rid=rId} = Table rname $ Just $ rname ++ show rId

tableName :: Relation -> String
tableName Relation{name=rname, rid=0}   = rname
tableName Relation{name=rname, rid=rId} = rname ++ (show $ rId)

_withClauses :: (Map String DatalogStmt) -> [TupleAttrRef] -> [ CTE ]
_withClauses rules body = map (uncurry CTE) $ catMaybes $ map findRules $ findTables
  where
    findTables :: [ String ]
    findTables = catMaybes $ map (\x -> case x of
                                          Table name_ _ -> Just name_ -- TODO handle Table alias
                                          _ -> Nothing) $ _fromTables body
    findRules :: String -> Maybe (String, SelectStmt)
    findRules rname = (fmap (\x -> (rname, hogehoge x))) . (\x -> lookup x rules) $ rname
      where
        hogehoge :: DatalogStmt -> SelectStmt
        hogehoge stmt = genSQLAST rules stmt

_selectExprs :: [ TupleAttrRef ] -> [ TupleAttrRef ] -> [ SelectExpr ]
_selectExprs head body = catMaybes $ map ((fmap (\(h, r) -> SelectExpr (ColumnRef (tableName $ rel r) (attr r)) (Just $ attr h))).hage) head
  where
    hage r@TupleAttrRef{arg=Var vname} = Just (r, (varAttrs vname body)!!0) -- !!0は重複ロジックなのでリファクタ必要
    hage _ = Nothing


joins :: [ TupleAttrRef ] -> [ TupleAttrRef ] -> [[ TupleAttrRef ]]
joins head body = map (\vname -> varAttrs vname body) $ varNames head body
-- TODO groupBy を使ってもっと綺麗に書きたいよー
-- TODO ?が付いているfactは除外しないと...

constraints :: [ TupleAttrRef ] -> [ TupleAttrRef ]
constraints body = filter (isNothing.varName) body

_whereClause :: [ TupleAttrRef ] -> [ TupleAttrRef ] -> [ Predicate ]
_whereClause head body = catMaybes $ map joinEquals (concat $ map something $ joins head body) ++ map constraintEquals (constraints body)
  where
    joinEquals :: (TupleAttrRef, TupleAttrRef) -> Maybe Predicate
    joinEquals (r1@TupleAttrRef{arg=Var _}, r2@TupleAttrRef{arg=Var _}) = Just $ Equal (ColumnRef (tableName $ rel r1) (attr r1))
                                                                                 (ColumnRef (tableName $ rel r2) (attr r2))
    joinEquals _ = Nothing

    constraintEquals :: TupleAttrRef -> Maybe Predicate
    constraintEquals r@TupleAttrRef{arg=Atom aname} = Just $ Equal (ColumnRef (tableName $ rel r) (attr r))
                                                      (SqlStr (aname))
    constraintEquals _ = Nothing

genSimpleSelectAST :: Map String DatalogStmt -> DatalogStmt -> SelectStmt
genSimpleSelectAST _ (DatalogFact _) = undefined
genSimpleSelectAST rules (DatalogRule ((DatalogHead head, DatalogBody body):[])) = SelectStmt {
  withClauses = _withClauses rules body,
  selectExprs = _selectExprs head body,
  fromTables = _fromTables body,
  whereClause = _whereClause head body
}
genSimpleSelectAST rules (DatalogRule rs) = foldl1 Union $ map (\r -> genSimpleSelectAST rules (DatalogRule(r:[]))) rs
genSimpleSelectAST rules (DatalogQuery (DatalogHead head) (DatalogBody body)) = SelectStmt {
  withClauses = _withClauses rules body,
  selectExprs = _selectExprs head body,
  fromTables = _fromTables body,
  whereClause = _whereClause head body
}
  where 
    --freeVarNames :: [String]
    --freeVarNames = nub $ catMaybes $ map varName $ head

    --freeVars :: [ TupleAttrRef ]
    --freeVars = filter isVarRef head


something :: [a] -> [(a,a)] -- TODO rename
something [] = []
something (_:[]) = [] -- 変数への参照がひとつしかない場合は空
something (x:xs) = reverse $ somethingHoge x xs []
  where somethingHoge _ [] piyo = piyo
        somethingHoge y (y':ys) piyo = somethingHoge y ys $ (y, y') : piyo
