module Step where

import Data.List

occur l = map (length . flip elemIndices l) l
funcOccur l = map ( \x -> if x > 1 then fst else snd)(occur l)
apply = zipWith (curry (\el -> fst el (snd el)))

step x y = if t == y then t else step x t
  where t = apply (funcOccur y) (zip x y)

correct xs = foldl (\acc x -> acc && x == 1) True (occur xs)

correctKey x = correct $ map fst x
correctVal x = correct $ map snd x
correctM x = correctKey x && correctVal x

keyList l = map fst $ sortOn fst l
valList l = map snd $ sortOn fst l

stepM x y = zip kL (step vL1 vL2)
  where kL = keyList x
        vL1 = valList x
        vL2 = valList y

stepMGuard x y = if kL1 == kL2 && correctM x then zip kL1 (step vL1 vL2) else x
  where kL1 = keyList x
        kL2 = keyList y
        vL1 = valList x
        vL2 = valList y
