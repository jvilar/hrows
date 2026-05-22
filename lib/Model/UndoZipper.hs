module Model.UndoZipper (
    UndoZipper
    , current
    , back
    , forward
    , push
    , mkUndoZipper
) where

-- | A simple undo/redo structure, implemented as a zipper.
data UndoZipper a = UndoZipper [a] a [a]

-- | Create a new UndoZipper with the given initial value.
mkUndoZipper :: a -> UndoZipper a
mkUndoZipper x = UndoZipper [] x []

-- | The current value.
current :: UndoZipper a -> a
current (UndoZipper _ c _) = c

-- | Move one Step back, return Nothing if there is no previous value.
back :: UndoZipper a -> Maybe (UndoZipper a)
back (UndoZipper (u:us) c rs) = Just $ UndoZipper us u (c:rs)
back _ = Nothing

-- | Move one Step forward, return Nothing if there is no next value.
forward :: UndoZipper a -> Maybe (UndoZipper a)
forward (UndoZipper us c (r:rs)) = Just $ UndoZipper (c:us) r rs
forward _ = Nothing

-- | Add a new value, remove all forward values.
push :: a -> UndoZipper a -> UndoZipper a
push m (UndoZipper us c _) = UndoZipper (c:us) m []
