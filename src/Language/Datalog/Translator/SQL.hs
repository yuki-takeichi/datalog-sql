module Language.Datalog.Translator.SQL (
  genSQLAST
) where

import Prelude hiding (lookup, head)
import Data.List ({-groupBy, -}nub)
import Data.Maybe (catMaybes, isNothing)
import Data.Map (Map, lookup)

import Data.Datalog.AST

genSQLAST :: Map String DatalogStmt -- TODO MapのvalueがDatalotStmtだとlookupで毎回分岐の実装を強いられるのが辛いので変更する。
             -> DatalogStmt
             -> SelectStmt
genSQLAST rules stmt = flattenWithClauses $ foldl1 Union $ [ genSimpleSelectAST rules stmt ]
  where
    flattenWithClauses :: SelectStmt -> SelectStmt
    flattenWithClauses = id -- TODO これを入れると読みやすいっていう人と読みにくいっていう人の2パターンいそう。optionalにできるとよい。


genSimpleSelectAST :: Map String DatalogStmt -> DatalogStmt -> SelectStmt
genSimpleSelectAST _ (DatalogFact _) = undefined
genSimpleSelectAST _ (DatalogRule []) = undefined
genSimpleSelectAST rules (DatalogQuery (DatalogHead head) (DatalogBody body)) = SelectStmt {
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
            hogehoge :: DatalogStmt -> SelectStmt
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
genSimpleSelectAST _ (DatalogRule _) = undefined

something :: [a] -> [(a,a)] -- TODO rename
something [] = []
something (_:[]) = [] -- 変数への参照がひとつしかない場合は空
something (x:xs) = reverse $ somethingHoge x xs []
  where somethingHoge _ [] piyo = piyo
        somethingHoge y (y':ys) piyo = somethingHoge y ys $ (y, y') : piyo

tableName :: Relation -> String
tableName r = (name r) ++ (show $ rid r)

