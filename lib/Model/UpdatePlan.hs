{-# LANGUAGE OverloadedStrings
           , TupleSections #-}

module Model.UpdatePlan ( UpdatePlan
                        , mkUpdatePlan
                        , updateAll
                        , updateField
                        ) where

import Data.IntMap(IntMap)
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import Data.Maybe(fromJust, isJust)
import Data.List(foldl', sortOn)

import Model.Expression
import Model.Expression.Evaluation
import Model.TopoSort
import Model.Row

data UpdatePlan = UpdatePlan { expressions :: [Maybe Expression]
                             , influences :: IntMap [FieldPos]
                             , updateOrder :: [FieldPos]
                             , cycled :: [FieldPos]
                             } deriving Show

enumerate :: [a] -> [(Int, a)]
enumerate = zip [0..]

mkUpdatePlan :: [Maybe Expression] -> UpdatePlan
mkUpdatePlan exps = let
    dependencies = enumerate $ map (map fromIntegral . maybe [] getPositions) exps

    pars = foldl' updatePars (IM.fromList . enumerate $ replicate (length exps) []) dependencies
    updatePars im (i, vars) = foldr (IM.adjust (i:)) im vars

    edges = foldl' updateEdges [] dependencies
    isExp = IS.fromList [ i | (i, e) <- enumerate exps, isJust e ]
    updateEdges l (i, vars) = [(v, i) | v <- vars] ++ l
    (order, cycles) = toposort $ mkGraph edges
    goodOrder = filter (`IS.member` isExp) order ++ [ i | (i, []) <- dependencies, i `IS.member` isExp ]
    in UpdatePlan { expressions = exps
                  , influences = closureUpdates  pars
                  , updateOrder = map fromIntegral goodOrder
                  , cycled = map fromIntegral cycles
                  }

closureUpdates :: IntMap [Int] -> IntMap [FieldPos]
closureUpdates isParameterOf = let
    ks = IM.keys isParameterOf
    edges = foldl' updateEdges [] ks
    updateEdges l i = [(i, f) | f <- isParameterOf IM.! i] ++ l
    graph = mkGraph edges
    order = fst . toposort $ graph
    orderMap = IM.fromList (zip order [(1::Int) ..])
    initial = IM.fromList (zip ks $ repeat [])
    updateClosure n cl = let
          deps = tail $ dfs graph n
        in IM.insert n (sortOn (orderMap IM.!) deps) cl
    in IM.map (map fromIntegral) $ foldr updateClosure initial order

updateAll :: UpdatePlan -> [DataSource] -> Row -> Row
updateAll up dss r = foldr (changeRow $ mkError "Fórmula con dependencias circulares")
                 (foldl' (evaluateField up dss) r (updateOrder up)) (cycled up)

-- |Changes a 'Field' and Updates a 'Row'. Returns the new 'Row'
-- and a list of the positions of the fields that changed, including
-- the one responsible of the change.
updateField :: UpdatePlan -> Field -> FieldPos -> [DataSource] -> Row -> (Row, [FieldPos])
updateField up f n dss r = let
    deps = influences up IM.! fromIntegral n
    in (foldl' (evaluateField up dss) (changeRow f n r) deps, n:deps)

changeRow :: Field -> FieldPos -> Row -> Row
changeRow f n r = let
    (h, _:t) = splitAt (fromIntegral n) r
    in h ++ f : t

evaluateField :: UpdatePlan -> [DataSource] -> Row -> FieldPos -> Row
evaluateField up dss r f = let
    v = evaluate r dss (fromJust $ expressions up !! fromIntegral f)
    in changeRow v f r

