{-# LANGUAGE OverloadedStrings #-}


module Model.RowStore.UpdatePlan ( mkUpdatePlan ) where

import Data.IntMap(IntMap)
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import Data.List(foldl', sortOn)

import Model.Expression
import Model.RowStore.Base
import Model.RowStore.TopoSort

enumerate :: [a] -> [(Int, a)]
enumerate = zip [0..]

mkUpdatePlan :: [Maybe Expression] -> UpdatePlan
mkUpdatePlan exps = let
    dependencies = enumerate $ map (map fromIntegral . maybe [] getPositions) exps

    pars = foldl' updatePars (IM.fromList . enumerate $ replicate (length exps) []) dependencies
    updatePars im (i, vars) = foldr (IM.adjust (i:)) im vars

    edges = foldl' updateEdges [] dependencies
    isExp = IS.fromList [ i | (i, Just _) <- enumerate exps ]
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

