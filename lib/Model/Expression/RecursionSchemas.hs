module Model.Expression.RecursionSchemas where

import Control.Arrow((>>>), (<<<))
import Data.Function((&))


newtype Fix t = In { out :: t (Fix t) }

type Algebra f a = f a -> a

bottomUp :: Functor a => (Fix a -> Fix a) -> Fix a -> Fix a
bottomUp f = out >>> fmap (bottomUp f) >>> In >>> f

bottomUpM :: (Monad m, Traversable a) => (Fix a -> m (Fix a)) -> Fix a -> m (Fix a)
-- bottomUpM f = out >>> fmap (bottomUp f) >>> In >>> f
bottomUpM f v = mapM (bottomUpM f) (out v) >>= f . In

topDown  :: Functor a => (Fix a -> Fix a) -> Fix a -> Fix a
topDown f = In <<< fmap (topDown f) <<< out <<< f

cata :: Functor f => Algebra f a -> Fix f -> a
cata f = out >>> fmap (cata f) >>> f

type AlgebraM m f a = f a -> m a

cataM :: (Traversable f, Monad m) => AlgebraM m f a -> Fix f -> m a
cataM f v = mapM (cataM f) (out v) >>= f

hookedCataM :: (Traversable f, Monad m) => (f (Fix f) -> m a -> m a) -> AlgebraM m f a -> Fix f -> m a
hookedCataM hook f (In v) = hook v $ mapM (hookedCataM hook f) v >>= f

type RAlgebra f a = Fix f -> f a -> a

para :: Functor f => RAlgebra f a -> Fix f -> a
para rAlg t = out t & fmap (para rAlg) & rAlg t