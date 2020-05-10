module Model.TopoSort ( Graph
                      , emptyGraph
                      , addEdge
                      , mkGraph
                      , toposort
                      , dfs
                      ) where

import Control.Monad(unless)
import Control.Monad.State.Strict(evalStateT, gets, modify, StateT)
import Control.Monad.Trans(lift)
import Control.Monad.Writer(execWriter, tell, Writer)
import Data.IntMap.Strict(IntMap)
import qualified Data.IntMap.Strict as IM
import Data.IntSet(IntSet)
import qualified Data.IntSet as IS
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
    in go noPreds (succs g) (foldr IM.delete (preds g) noPreds)

missingOrEmpty :: IntMap [Int] -> [Int] -> [Int]
missingOrEmpty m = filter cond
    where cond = maybe True null . flip IM.lookup m

go :: [Int] -> IntMap [Int] -> IntMap [Int] -> ([Int], [Int])
go [] _ preds = ([], IM.keys preds)
go (n:ns) succs preds = let
      sn = succs IM.! n
      np = filter ((==1).length.(preds IM.!)) sn
      preds' = foldr (IM.update $ update n) preds sn
      update _ [_] = Nothing
      update n l = Just $ delete n l
      (good, cycle) = go (np ++ ns) succs preds'
    in (n:good, cycle)

-- |Returns the visited vertices of the graph in a DFS departing from
-- the given vertex.
dfs :: Graph -> Int -> [Int]
dfs g n = execWriter (evalStateT (go n) IS.empty)
    where go :: Int -> StateT IntSet (Writer [Int]) ()
          go n = do
              seen <- gets (IS.member n)
              unless seen $ do
                  lift $ tell [n]
                  modify (IS.insert n)
                  mapM_ go (succs g IM.! n)

emptyGraph :: Graph
emptyGraph = Graph [] IM.empty IM.empty

addEdge :: (Int, Int) -> Graph -> Graph
addEdge (u, v) g = g { vertices = addL u . addL v $ vertices g
                     , succs = IM.alter (add v) u $
                               IM.alter addEmpty v (succs g)
                     , preds = IM.alter (add u) v $
                               IM.alter addEmpty u (preds g)
                     }
                   where add x = Just . maybe [x] (addL x)
                         addEmpty = Just . fromMaybe []
                         addL x l | x `elem` l = l
                                  | otherwise = x : l

mkGraph :: [(Int, Int)] -> Graph
mkGraph = foldr addEdge emptyGraph

