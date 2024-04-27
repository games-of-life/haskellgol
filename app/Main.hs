module Main where

import Data.List (group, sort)
import qualified Data.Set as Set
import Raylib.Core (clearBackground, initWindow, closeWindow, windowShouldClose)
import Raylib.Core.Shapes (drawRectangle)
import Raylib.Core.Text (drawFPS)
import Raylib.Util (drawing)
import Raylib.Util.Colors (black, white)
import System.Random

width :: Int
width = 800

height :: Int
height = 600

boxDimension :: Int
boxDimension = 10

data Coords = Coords Int Int deriving (Show, Eq, Ord)

type Field = Set.Set Coords

randomCoords :: Int -> Int -> IO Coords
randomCoords h w = do
  x <- randomRIO (0, h)
  y <- randomRIO (0, w)
  return $ Coords x y

randomField :: Int -> Int -> Double -> IO Field
randomField h w prob = do
  let am = round (fromIntegral h * fromIntegral w * prob)
  coords <- sequence [randomCoords h w | _ <- [1 .. am]]
  return (Set.fromList coords)

isAlive :: Field -> Int -> Int -> Bool
isAlive cells x y = Set.member (Coords x y) cells

drawField :: Field -> Int -> Int -> Int -> Int -> Int -> IO ()
drawField f x y h w dim
  | x == h = drawField f 0 (y + 1) h w dim
  | y == w = return ()
  | otherwise = do
      drawRectangle (x * dim) (y * dim) (dim - 1) (dim - 1) (if isAlive f x y then white else black)
      drawField f (x + 1) y h w dim

getMooreNeighborhood :: Coords -> [Coords]
getMooreNeighborhood (Coords x y) = [Coords (x + dx) (y + dy) | dx <- [-1 .. 1], dy <- [-1 .. 1], (dx, dy) /= (0, 0)]

frequencies :: [Coords] -> [(Coords, Int)]
frequencies lc = map (\c -> (head c, length c)) $ group $ sort lc

filterOutOfBounds :: [Coords] -> Int -> Int -> [Coords]
filterOutOfBounds c w h = filter (\(Coords x y) -> not (x < 0 || y < 0 || x >= w || y >= h)) c

runStep :: Field -> Int -> Int -> Field
runStep f w h =
  Set.fromList $
    map fst $
      filter (\(c, fr) -> fr == 3 || (fr == 2 && Set.member c f)) $
        frequencies $
          filterOutOfBounds (concatMap getMooreNeighborhood f) w h

loop :: Field -> Int -> Int -> IO ()
loop f box_width box_height = do
  clearBackground black
  drawing $ do
    drawField f 0 0 box_width box_height boxDimension
    drawFPS 20 20
  shouldClose <- windowShouldClose
  if shouldClose then
    return ()
  else
    loop (runStep f box_width box_height) box_width box_height

main :: IO ()
main = do
  let box_width = width `div` boxDimension
  let box_height = height `div` boxDimension
  field <- randomField box_width box_height 0.5
  window <- initWindow width height "Game of life"
  loop field box_width box_height
  closeWindow window
