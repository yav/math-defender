{-# LANGUAGE RecordWildCards #-}
import Allegro
import Allegro.Graphics
import Allegro.Display
import Allegro.EventQueue
import Allegro.Keyboard
import Allegro.Primitives
import Allegro.Transform
import Allegro.Timer
import Data.Array
import Data.Integer.SAT
import Control.Monad(forM_)
import Text.Show.Pretty

blue    = Color 0 0 0.80 1
yellow  = Color 1 0.80 0 1
white   = Color 1 1 1 1

border  = 3

rect c x y w h =
  do forM_ [ x .. x + w ] $ \v ->
       drawLine (Line (v,y) (v,y+h)) c 0
     forM_ [ y .. y + h ] $ \v ->
       drawLine (Line (x,v) (x + w,v)) c 0


data P = P { w1, h1, x2, y2, w2, h2 :: Integer } deriving Show

drawP :: P -> IO ()
drawP P { .. } =
  do rect blue   0                 0
                 (fromIntegral w1) (fromIntegral h1)
     rect yellow (fromIntegral x2) (fromIntegral y2)
                 (fromIntegral w2) (fromIntegral h2)

vw1 : vh1 : vx2 : vy2 : vw2 : vh2 : _ = map (Var . toName) [ 0 .. ]

basicConstraints :: [Prop]
basicConstraints = foldr (:) []
                 $ map isWidth [ vw1, vh1, vw2, vh2 ] ++
                   map isPos   [ vx2, vy2 ]
  where
  isPos a       = between a (K (-10)) (K 10)
  isWidth a     = between a (K 1) (K 5)
  between x a b = a :<= x :&& x :<= b

pFromAssign :: [(Int,Integer)] -> P
pFromAssign as = P (get 0) (get 1) (get 2) (get 3) (get 4) (get 5)
  where
  get x = case lookup x as of
            Nothing -> 0
            Just x  -> x

nonTriv :: Prop -> Bool
nonTriv p =
  case checkSat (assert p noProps) of
    Nothing -> False
    Just _  ->
      case checkSat (assert (Not p) noProps) of
        Nothing -> False
        Just _  -> True


exact :: P -> Prop
exact P { .. } = foldr1 (:&&)
                  [ vw1 :== K w1, vh1 :== K h1
                  , vx2 :== K x2, vy2 :== K y2
                  , vw2 :== K w2, vh2 :== K h2 ]

drawScreen mbp1 mbp2 =
  do clearToColor (Color 0 0 0 0)
     mb mbp1 $ \p1 -> withTransformSRT (5,5) 0 (100,100) (drawP p1)
     drawLine (Line (200,0) (200,200)) white 3
     mb mbp2 $ \p2 -> withTransformSRT (5,5) 0 (300,100) (drawP p2)
     flipDisplay

  where mb Nothing _  = return ()
        mb (Just x) f = f x


main :: IO ()
main =
  allegro $
  withDisplay FixedWindow 640 480 $ \_ ->
    do q <- createEventQueue
       registerEventSource q =<< installKeyboard

       let mk = foldr1 (:&&)
       -- let p = mk [ vw1 :== K 5, vw2 :== vw1, vh1 :> K 2, vx2 :== K 0 ]
       let p = mk [ vw1 :>= vx2 :+ vw2, vh1 :>= vy2 :+ vh2
                  , vx2 :>= K 0, vy2 :>= K 0 ]

           stGood = foldr assert noProps $ p     : basicConstraints
           stBad  = foldr assert noProps $ Not p : basicConstraints

           enum = cycle . map (pFromAssign . slnCurrent)
                        . concatMap slnEnumerate . allSolutions

           allGood = enum stGood
           allBad  = enum stBad

       t <- createTimer (1/2)
       registerEventSource q t
       startTimer t
       let go gs bs =
              do putStrLn "Waiting..."
                 ev <- waitForEvent q
                 case ev of
                      KeyDown k
                        | evKey k == key_ESCAPE -> return ()
                      Time _ ->
                         do drawScreen (Just $ head gs) (Just $ head bs)
                            go (tail gs) (tail bs)
                      _ -> go gs bs
       go allGood allBad

