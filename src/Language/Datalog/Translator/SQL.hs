module Language.Datalog.Translator.SQL (
  genSQLAST
) where

import Prelude hiding (lookup, head)
import Data.List (nub)
import Data.Maybe (catMaybes, isNothing)
import Data.Map (Map, lookup, delete)

import Data.Datalog.AST

type VarName = String

genSQLAST :: Map String DatalogStmt
             -> DatalogStmt
             -> SelectStmt
genSQLAST rules stmt = flattenWithClauses $ foldl1 Union $ [ genSimpleSelectAST rules stmt ]
  where
    flattenWithClauses :: SelectStmt -> SelectStmt
    flattenWithClauses = id

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
_withClauses rules body = map (uncurry CTE) $ catMaybes $ map findRule $ findTables
  where
    findTables :: [ String ]
    findTables = catMaybes $ map (\x -> case x of
                                          Table name_ _ -> Just name_
                                          _ -> Nothing) $ _fromTables body
    findRule :: String -> Maybe (String, SelectStmt)
    findRule rname = (fmap (\x -> (rname, genInnerSelect x))) . (\x -> lookup x rules) $ rname
      where
        genInnerSelect :: DatalogStmt -> SelectStmt
        genInnerSelect rule | isRecursive rule = genInnerRecursiveSelect rule
        genInnerSelect rule | otherwise        = genInnerNonRecursiveSelect rule
        genInnerNonRecursiveSelect :: DatalogStmt -> SelectStmt
        genInnerNonRecursiveSelect r = genSQLAST rules r
        genInnerRecursiveSelect :: DatalogStmt -> SelectStmt
        genInnerRecursiveSelect (DatalogFact _) = undefined
        genInnerRecursiveSelect (DatalogQuery _ _) = undefined
        genInnerRecursiveSelect (DatalogRule rs) = UnionAll noRecs recs
          where
            noRecs = foldl1 Union $ map (\r -> genSimpleSelectAST rules (DatalogRule(r:[]))) $ filter (not . _isRecursive) rs
            recs = foldl1 UnionAll $ map (\r -> genSimpleSelectAST rulesWithoutItself (DatalogRule(r:[]))) $ filter _isRecursive rs
              where
                rulesWithoutItself = let (DatalogHead h, DatalogBody _) = rs!!0
                                     in delete (name $ rel $ h!!0) rules

        isRecursive :: DatalogStmt -> Bool
        isRecursive (DatalogFact _) = False
        isRecursive (DatalogQuery _ _) = False
        isRecursive (DatalogRule rs) = any _isRecursive rs

        _isRecursive :: (DatalogHead, DatalogBody) -> Bool
        _isRecursive (DatalogHead _head, DatalogBody _body) = target `elem` bodyRels
          where
            target = name $ rel $ _head!!0
            bodyRels = nub $ map (name . rel) _body

_selectExprs :: [ TupleAttrRef ] -> [ TupleAttrRef ] -> [ SelectExpr ]
_selectExprs head body = catMaybes $ map ((fmap (\(h, r) -> SelectExpr (ColumnRef (tableName $ rel r) (attr r)) (Just $ attr h))).hage) head
  where
    hage r@TupleAttrRef{arg=Var vname} = Just (r, (varAttrs vname body)!!0)
    hage _ = Nothing


joins :: [ TupleAttrRef ] -> [ TupleAttrRef ] -> [[ TupleAttrRef ]]
joins head body = map (\vname -> varAttrs vname body) $ varNames head body

constraints :: [ TupleAttrRef ] -> [ TupleAttrRef ]
constraints body = filter (isNothing.varName) body

_whereClause :: [ TupleAttrRef ] -> [ TupleAttrRef ] -> [ Predicate ]
_whereClause head body = catMaybes $ map joinEquals (concat $ map pairsWithFirstElement $ joins head body) ++ map constraintEquals (constraints body)
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

pairsWithFirstElement :: [a] -> [(a,a)]
pairsWithFirstElement [] = []
pairsWithFirstElement (_:[]) = []
pairsWithFirstElement (x:xs) = reverse $ _pairsWithFirstElement x xs []
  where _pairsWithFirstElement _ [] piyo = piyo
        _pairsWithFirstElement y (y':ys) piyo = _pairsWithFirstElement y ys $ (y, y') : piyo

