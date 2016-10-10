{-# LANGUAGE TupleSections #-}

module Model.UpdatePlan ( UpdatePlan
                        , mkUpdatePlan
                        , updateAll
                        , updateField
                        ) where

import Data.IntMap(IntMap)
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import Data.Maybe(fromJust, isJust)
import Data.List(foldl', sortOn)

import Model.Expression
import Model.TopoSort
import Model.Parser
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
    dependencies = enumerate $ map (maybe [] getPositions) exps

    pars = foldl' updatePars (IM.fromList . enumerate $ replicate (length exps) []) dependencies
    updatePars im (i, vars) = foldr (IM.adjust (i:)) im vars

    edges = foldl' updateEdges [] dependencies
    isExp = IS.fromList [ i | (i, e) <- enumerate exps, isJust e ]
    updateEdges l (i, vars) = [(v, i) | v <- vars] ++ l
    (order, cycles) = toposort $ mkGraph edges
    in UpdatePlan { expressions = exps
                  , influences = closureUpdates pars
                  , updateOrder = filter (`IS.member` isExp) order
                  , cycled = cycles
                  }

closureUpdates :: IntMap [FieldPos] -> IntMap[FieldPos]
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
    in foldr updateClosure initial order

updateAll :: UpdatePlan -> Row -> Row
updateAll up r = foldr (changeRow $ mkError "Formula con dependencias circulares")
                 (foldl' (evaluateField up) r (updateOrder up)) (cycled up)

-- |Changes a 'Field' and Updates a 'Row'. Returns the new 'Row'
-- and a list of the positions of the fields that changed, including
-- the one responsible of the change.
updateField :: UpdatePlan -> Field -> FieldPos -> Row -> (Row, [FieldPos])
updateField up f n r = let
    deps = influences up IM.! n
    in (foldl' (evaluateField up) (changeRow f n r) deps, n:deps)

changeRow :: Field -> FieldPos -> Row -> Row
changeRow f n r = let
    (h, _:t) = splitAt n r
    in h ++ f : t

evaluateField :: UpdatePlan -> Row -> FieldPos -> Row
evaluateField up r f = let
    v = evaluate r (fromJust $ expressions up !! f)
    in changeRow v f r
 
