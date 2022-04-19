module Space where

class Action a where
    actToInt :: a -> Int

class Space a where
    init :: a
    next :: a -> a
    actionInt :: Int -> a -> a
    action :: (Action b) => b -> a -> a
    action x  = actionInt $ actToInt x 
