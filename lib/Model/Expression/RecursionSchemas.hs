module Model.Expression.RecursionSchemas where

import Control.Arrow((>>>), (<<<), (&&&))

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

type RAlgebra f a = f (Fix f, a) -> a

para :: Functor f => RAlgebra f a -> Fix f -> a
para rAlg = out >>> fmap (id &&& para rAlg) >>> rAlg
