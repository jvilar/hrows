module TopoSort ( Graph
                , emptyGraph
                , addEdge
                , mkGraph
                , toposort
                ) where

import Data.IntMap.Strict(IntMap)
import qualified Data.IntMap.Strict as IM
import Data.List(delete)
import Data.Maybe(fromMaybe)

data Graph = Graph { vertices :: [Int]
                   , succs :: IntMap [Int]
                   , preds :: IntMap [Int]
                   } deriving Show

-- |Given a 'Graph' returns the vertices in topological order. Two
-- pairs, the first with the vertices that can be visited in
-- topological order, the second with those involved in a cycle.
toposort :: Graph -> ([Int], [Int])
toposort g = let
    noPreds = missingOrEmpty (preds g) (vertices g)
    in go noPreds (succs g) (preds g)

missingOrEmpty :: IntMap [Int] -> [Int] -> [Int]
missingOrEmpty m = filter cond
    where cond = maybe True null . flip IM.lookup m

go :: [Int] -> IntMap [Int] -> IntMap [Int] -> ([Int], [Int])
go [] _ preds = ([], IM.keys preds)
go (n:ns) succs preds = let
      sn = fromMaybe [] $ IM.lookup n succs
      np = filter ((==1).length.(preds IM.!)) sn
      preds' = foldr (IM.update $ update n) preds sn
      update n [_] = Nothing
      update n l = Just $ delete n l
      (good, cycle) = go (np ++ ns) succs preds'
    in (n:good, cycle)


emptyGraph = Graph [] IM.empty IM.empty

addEdge :: (Int, Int) -> Graph -> Graph
addEdge (u, v) g = g { vertices = addL u . addL v $ vertices g
                     , succs = IM.alter (add v) u (succs g)
                     , preds = IM.alter (add u) v (preds g)
                     }
                   where add x Nothing = Just [x]
                         add x (Just l) = Just (addL x l)
                         addL x l | x `elem` l = l
                                  | otherwise = x : l

mkGraph :: [(Int, Int)] -> Graph
mkGraph = foldr addEdge emptyGraph

