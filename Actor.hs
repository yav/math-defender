{-# LANGUAGE Safe #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
module Actor where

import Allegro.Graphics
import Allegro.Primitives
import Data.Integer.SAT
import Control.Monad(forM_,when)

import Data.Map (Map)
import qualified Data.Map as Map

data Actor  = Actor { part1, part2, dist :: (Integer,Integer) }

actorVars :: Actor -> Map Name Integer
actorVars Actor { .. } =
  Map.fromList $ zip (map toName [ 0 .. ])
    [ fst part1, snd part1, fst part2, snd part2, fst dist, snd dist ]



blue, yellow, white :: Color
blue    = Color 0 0.3 1 1
yellow  = Color 1 0.80 0 1
white   = Color 1 1 1 1

drawPart :: Color -> Point -> Integer -> IO ()
drawPart c (x,y) (fromIntegral -> height) =
  do forM_ [ x .. x + 1 ] $ \v ->
       drawLine (Line (v,y) (v, y + height)) c 0
     forM_ [ y .. y + height ] $ \v ->
       drawLine (Line (x,v) (x + 1,v)) c 0

drawActor :: [Bool] -> Actor -> IO ()
drawActor on Actor { .. } = mapM_ draw (zip3 on [ 0 .. ] parts)
{-
     let (fromIntegral -> dx, fromIntegral -> dy) = dist
     drawPart yellow (dx, dy) part2
     drawShape Rectangle { rectTopLeft = (0,0)
                         , rectBottomRight = (12,6)
                         , rectCurved = Nothing }
              (Color 1 1 1 1) (Outlined 0)
-}
  where
  parts = [ (Color 0.8 0   0   1, fst part1)
          , (Color 0.8 0.8 0   1, snd part1)
          , (Color 0   0.8 0   1, fst part2)
          , (Color 0   0.8 0.8 1, snd part2)
          , (Color 0   0   0.8 1, fst dist)
          , (Color 0   0.8 0.6 1, snd dist)
          ]

  draw (on, n, (c,p)) = drawPart c (2 * n, 0) (if on then p else 0)

vw1, vh1, vx2, vy2, vw2, vh2 :: Expr
vw1 : vh1 : vw2 : vh2 : vx2 : vy2 : _ = map (Var . toName) [ 0 .. ]

basicConstraints :: [Prop]
basicConstraints = foldr (:) []
                 $ map isWidth [ vw1, vh1, vw2, vh2 ] ++
                   -- map isPos   [ vx2, vy2 ]
                  [ vx2 :== K 6, vy2 :== K 0 ]
  where
  isPos a       = between a (K (-10)) (K 10)
  isWidth a     = between a (K 1) (K 5)
  between x a b = a :<= x :&& x :<= b

actorFromAssign :: [(Int,Integer)] -> Actor
actorFromAssign as =
  Actor { part1 = mk 0
        , part2 = mk 2
        , dist  = mk 4
        }
  where
  mk n = (get n, get (n + 1))
  get x = case lookup x as of
            Nothing -> 0
            Just y  -> y


