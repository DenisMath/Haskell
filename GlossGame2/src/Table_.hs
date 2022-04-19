{-# LANGUAGE TupleSections #-}
module Table_ where
import Data.Maybe
-- import Data.List
import Data.List

minusL y  = filter (not . (`elem` y)) 
shft [] = []
shft t@(x:xs) = zip t xs
slice l = head l : map (uncurry minusL) (shft l)

squareTable  = [(i,j) | i <- [(1::Float) .. 8], j <- [(1::Float)..8]]

distC x y =  sqrt ((fst x - fst y) ** 2 + (snd x - snd y) ** 2)

  
newtype Coord = Coord (Float, Float) deriving(Show,Eq)

class (Eq a) => Cell a where
  
  dist_ :: [a] -> a -> a -> Maybe Float
  lessCoord_ :: a -> a-> a-> Ordering  
  max_dist_ :: [a] -> a -> Maybe Float
  max_dist_ table x = maximum [dist_ table x y | y <- table]

  min_dist_ :: [a] -> a -> Maybe Float
  min_dist_ table x = minimum $ filter (Just 0 /=)  [dist_ table x y | y <- table]

  period_list_ :: Float -> [a] -> a -> [Float]
  period_list_ p table x = if isNothing(max_dist_ table x) then [] else
    let Just t =  max_dist_ table x in [0, p .. t]

  bound_in_ :: [a] -> [a] -> a -> Float -> [a]
  bound_in_ table y x f = filter ((Just f >=) . dist_ table x)  y

  bound_in_l_ :: Float -> [a] -> [a] -> a -> [[a]]
  bound_in_l_ p table y x = nub [ bound_in_ table y x t| t <- period_list_ p table x]

  slice_in_l_ :: Float -> [a] -> [a] -> a -> [[a]]
  slice_in_l_ p table y x =  map (sortBy (lessCoord_ x) ) $ slice $ bound_in_l_ p table y x
  
  bound_ :: [a] -> a -> Float -> [a]
  bound_ table  =  bound_in_ table table 

  bound_l_ :: Float -> [a] -> a -> [[a]]
  bound_l_ p table  =  bound_in_l_ p table table 

  slice_l_ :: Float -> [a] -> a -> [[a]]
  slice_l_ p table x =  map (sortBy (lessCoord_ x) ) $ slice $ bound_l_ p table x

  view_ :: Float -> [a] -> a -> Int -> Int -> a
  view_ p table x  level i = let l = slice_l_ p table x!!level in l !! mod i (length l)

  view_guard_ :: Float -> [a] -> a -> Int -> Int -> a
  view_guard_ p table x level i = let l = slice_l_ p table x!! min level (length (bound_l_ p table x) - 1)
   in l!! mod i (length l)

class (Cell a) => Table a where
  table :: [a]
  dist :: a -> a -> Maybe Float
  dist = dist_ table

  max_dist :: a -> Maybe Float
  max_dist  = max_dist_ table 

  min_dist :: a -> Maybe Float
  min_dist  = min_dist_ table 

  period_list :: Float -> a -> [Float]
  period_list p = period_list_ p table 

  bound_in :: [a] -> a -> Float -> [a]
  bound_in = bound_in_ table 

  bound_in_l :: Float -> [a] -> a -> [[a]]
  bound_in_l p = bound_in_l_ p table 

  slice_in_l :: Float -> [a] -> a -> [[a]]
  slice_in_l p = slice_in_l_ p table 
  
  bound :: a -> Float -> [a]
  bound  =  bound_ table 

  bound_l :: Float -> a -> [[a]]
  bound_l p =  bound_l_ p table 

  slice_l :: Float -> a -> [[a]]
  slice_l p = slice_l_ p table 

  view :: Float -> a -> Int -> Int -> a
  view p  = view_ p table 

  view_guard :: Float -> a -> Int -> Int -> a
  view_guard p = view_ p table 

class (Table a) => TableD a where
  discr :: a -> Float
  discr x = let Just t = min_dist x in (t/2)
  
  period_list_d :: a -> [Float]
  period_list_d x = period_list (discr x) x

  bound_in_l_d :: [a] -> a -> [[a]]
  bound_in_l_d y x = bound_in_l (discr x) y x

  slice_in_l_d :: [a] -> a -> [[a]]
  slice_in_l_d y x = slice_in_l (discr x) y x
  
  bound_l_d ::  a -> [[a]]
  bound_l_d x =  bound_l (discr x) x

  slice_l_d :: a -> [[a]]
  slice_l_d x = slice_l (discr x) x

  view_d :: a -> Int -> Int -> a
  view_d x = view (discr x) x 

  view_guard_d :: a -> Int -> Int -> a
  view_guard_d x = view_guard (discr x) x 

instance Cell Coord where
  dist_ table x@(Coord x_) y@(Coord y_) = if elem x table && elem y table then Just (distC x_ y_) else Nothing

  lessCoord_ = lessCoord


nextGex l t = map (\(x,y) -> (x+ t* cos (pi/3), y + sin (pi/3))) l

extGexList [] _ = []                  
extGexList x@(xh:xt) t = nextGex xh t:x

extGexListN 1 initL = initL
extGexListN n initL = let l = extGexListN (n-1) initL
                  in nextGex (head l) ((-1) **  fromIntegral (mod n 2)):l


buildGex n = concat $ extGexListN n [map (,0.0) [1 .. fromIntegral n]]

buildSquare n = [(i,j) | i <- [1 .. fromIntegral n], j <- [1 .. fromIntegral n]]

squareTableCoord = map Coord $ buildSquare 20
gexTableCoord = map Coord $ buildGex 20
                  
instance Table Coord where
  table = squareTableCoord

instance TableD Coord where

cos_ (Coord (0,0)) = 1
cos_ (Coord (x,y)) = x/ sqrt (x*x + y*y)

sin_ (Coord (0,0)) = 0
sin_ (Coord (x,y)) = y/sqrt (x*x + y*y)

numCell x@(Coord (x1,y1))
  | sin_ x>=0 && cos_ x > 0 = 0
  | sin_ x>0 && cos_ x <= 0 = 1
  | sin_ x <= 0 && cos_ x < 0 = 2
  | sin_ x < 0 && cos_ x >= 0 = 3
  | otherwise = 4
  

    
lessCoord x@(Coord (x1,y1)) y@(Coord (x2,y2)) z@(Coord (x3,y3))
    | (cos_ x_ == cos_ y_) &&   (sin_ x_ == sin_ y_) = EQ
    | numCell x_ < numCell y_ = GT
    | numCell y_ < numCell x_ = LT
    | numCell x_< 2 = if cos_ x_ < cos_ y_ then LT else GT
    | numCell x_ < 4 = if cos_ x_ > cos_ y_ then LT else GT
    | otherwise   = EQ
    where
      x_ = Coord (x2-x1,y2-y1)
      y_ = Coord (x3-x1,y3-y1)
      

