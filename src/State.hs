module State (
             -- *Types
             State

             -- *Constants
             , state0
)

data State = State { model :: Model
                   , pos :: Int
                   }

state0 :: State
state0 = State { model = model0
               , pos = 0
               }

class StateUpdater t where
    update :: State -> t -> State
