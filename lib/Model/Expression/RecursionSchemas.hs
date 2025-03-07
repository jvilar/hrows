module Model.Expression.RecursionSchemas where

import Control.Arrow((>>>), (<<<), (&&&))

newtype Fix t = In { out :: t (Fix t) }

type Algebra f a = f a -> a

bottomUp :: Functor a => (a (Fix a) -> a (Fix a)) -> Fix a -> Fix a
bottomUp f = out >>> fmap (bottomUp f) >>> f >>> In

bottomUpM :: (Monad m, Traversable a) => (a (Fix a) -> m (a (Fix a))) -> Fix a -> m (Fix a)
bottomUpM f v = mapM (bottomUpM f) (out v) >>= fmap In . f

topDown  :: Functor a => (a (Fix a) -> a (Fix a)) -> Fix a -> Fix a
topDown f = In <<< fmap (topDown f) <<< f <<< out

cata :: Functor f => Algebra f a -> Fix f -> a
cata f = out >>> fmap (cata f) >>> f

type RAlgebra f a = f (Fix f, a) -> a

para :: Functor f => RAlgebra f a -> Fix f -> a
para rAlg = out >>> fmap (id &&& para rAlg) >>> rAlg
