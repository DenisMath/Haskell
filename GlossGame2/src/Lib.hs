module Lib where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.IO.Game
import Table_


data ObjType = ObjType {coord :: (Int,Int), imageType :: String}
newtype World = World [ObjType]

orangered, orangered2, orangered3 :: Color
orangered = makeColor 1.0 0.251 0.0 0.7
orangered2 = makeColor 1.0 0.251 0.0 0.5
orangered3 = makeColor 1.0 0.251 0.0 0.3

zipColor l = let t = length l in 
  zip 
  [ makeColor (1 - (fromIntegral i/fromIntegral (3*t)))
  (1 - (fromIntegral i/fromIntegral (2*t)))
  (1 - (fromIntegral i/fromIntegral t)) 0.9 | i <- [0 .. (t-1)]]  l

zipColor1 l = let t = length l in zip (cycle [makeColor 0 0 0 0.1, makeColor 0 1 0 0.8, red, orange])  l

defaultObj x y= ObjType {coord = (x,y), imageType = "Green"}

objToPict :: ObjType -> Picture
objToPict x = let c@(cx,cy) = coord x
                  x_str = imageType x
                  in  translate (fromIntegral cx) (fromIntegral cy) $ imageTypeToPict x_str
  

imageTypeToPict x = case x of
  "Red" -> color red $ circle 5
  "Black" -> color black $ line [(-5,5),(5,5),(5,-5),(-5,-5),(-5,5)]
  _ -> color green $ line [(-5,5),(5,5),(5,-5),(-5,-5),(-5,5)]

cll = gexTableCoord !! 210

zeroSt x = if x>0 then x else 0

