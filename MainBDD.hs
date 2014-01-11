module Main where

import Data.Map ( Map )
import qualified Data.Map as Map
import Data.Set ( Set )
import qualified Data.Set as Set
import Data.List(intercalate)
import BDD

import Allegro(allegro)
import Allegro.Display
import Allegro.Keyboard
import Allegro.EventQueue
import Allegro.Primitives
import Allegro.Font(loadFont,Font,drawText,Alignment(..))

import System.Random
import Control.Monad

type Name = String
data P    = P { pSchema :: !Bool, pName :: !Name, pParams :: ![Name] }
            deriving (Eq,Ord)

type Loc    = (Int,Int)
data Elem   = EBool Bool | EPrim P | ERef Loc
data Layout = Layout { refLocs :: Map Ref Loc, theLayout :: [(Loc,Elem)] }

layout :: Loc -> F P -> Layout -> (Layout, Int)
layout loc (Bool b) l = (l { theLayout = (loc, EBool b) : theLayout l }, 1)
layout loc (Ref (If p f1 f2) r) l =
  case Map.insertLookupWithKey (\_ _ _ -> loc) r loc (refLocs l) of
    (Just loc1, _) -> (l { theLayout = (loc, ERef loc1) : theLayout l }, 1)
    (Nothing, m) ->
      let l1 = Layout { refLocs = m, theLayout = (loc, EPrim p) : theLayout l }
          (l2, w)  = layout (down 1 loc) f1 l1
          wThis    = max 1 w
          (l3, w') = layout (right wThis loc) f2 l2
      in (l3, wThis + w')
  where
  down n  (x,y) = (x,y+n)
  right n (x,y) = (x+n,y)


drawElem :: Font -> (Loc,Elem) -> IO ()
drawElem font (loc, e) =
  case e of
    EBool b ->
      let c = if b then Color 0 1 0 0 else Color 1 0 0 0
      in drawShape (circle pos (box/4)) c Filled
    ERef loc1 ->
      let pos1 = cvt loc1
          pos2 = (fst pos1, snd pos)
      in drawLine (Spline pos pos2 pos2 pos1) white 0
    EPrim p -> drawP font pos p

  where
  white = Color 1 1 1 0
  box = 50
  pos       = cvt loc
  cvt (a,b) = (cvt1 a, cvt1 b)
  cvt1 x    = fromIntegral x * box

drawP :: Font -> Point -> P -> IO ()
drawP font pos p = drawText font color pos AlignCenter (labelP p)
  where color = Color 1 1 1 0

labelP :: P -> String
labelP p = pref ++ pName p ++ suff
  where
  pref = if pSchema p then "?" else ""
  suff = case pParams p of
           [] -> ""
           ps -> "(" ++ intercalate "," ps ++ ")"



drawF :: Font -> F P -> IO ()
drawF font f =
  let (l, _) = layout (1,1) f Layout { refLocs = Map.empty, theLayout = [] }
  in mapM_ (drawElem font) (theLayout l)


--------------------------------------------------------------------------------

screenW :: Int
screenW = 1024

screenH :: Int
screenH = 768

var :: String -> HF P
var x = fVar P { pName = x, pParams = [], pSchema = False }

randF :: IO (F P)
randF =
  do let n = 100
     x <- replicateM n $ do x <- randomRIO (1,100)
                            fix <- randomIO
                            let toVar :: Integer -> HF P
                                toVar x = fVar P { pName = show x
                                                 , pParams = []
                                                 , pSchema = fix }
                            pos <- randomIO
                            return $ if pos then toVar x else fNot (toVar x)
     let mk a b = do o  <- randomRIO (1,3)
                     let op = case (o::Integer) of
                                1 -> fAnd
                                2 -> fOr
                                _ -> fImplies
                     return (op a b)


     done `fmap` foldM mk (head x) (tail x)


main :: IO ()
main =
  do let form = fVar (P False "A" ["x1"]) `fImplies`
                fVar (P False "B" ["x1"]) `fImplies`
                fVar (P True  "P" ["a0","x0"]) `fImplies`
                fVar (P True  "I" ["a1","x1"]) `fImplies`
                fVar (P False "C" ["a1[x1]"]) `fImplies`
                fVar (P True "I" ["a1","x1 - 1"])
        -- <- randF
     putStrLn (mkDot (done form))
     
{-
main =
  allegro $
  do font <- loadFont "resources/font.ttf" 12
     withDisplay FixedWindow screenW screenH $ \d ->
       do setWindowTitle d "Hello"
          q <- createEventQueue
          registerEventSource q =<< installKeyboard

          form <- randF -- = done $ fImplies (var "a") (var "b" `fOr` var "a")
          -- let form = done $ fImplies (var "a") (var "b" `fOr` var "a")



          drawF font form
          flipDisplay

          let go =
               do ev <- waitForEvent q
                  case ev of
                    KeyChar e ->
                      case evKeyChar e of
                        Just 'q' -> return ()
                        _        -> go
                    _ -> go

          go
-}

data DotS = DotS { dotNext :: Int, dotDone :: Set Int, dot :: [String] }

mkDot :: F P -> String
mkDot form = unlines $ "digraph {" : dot s ++ ["}"]
  where
  (_,s) = toDot form DotS { dotNext = 0, dotDone = Set.empty, dot = [] }

toDot :: F P -> DotS -> (String, DotS)
toDot (Bool x) s =
  ( node
  , DotS { dotNext = dotNext s + 1
         , dotDone = dotDone s
         , dot     = (node ++ " [label=\"\", color=" ++ color ++ "]") : dot s
         }
  )
  where node  = "X_" ++ show (dotNext s)
        color = show (if x then "green" else "red")


toDot (Ref _ r) s | r `Set.member` dotDone s = (show r, s)
toDot (Ref (If p f1 f2) r) s =
  ( me
  , s3 { dot = (me ++ " -> " ++ f1' ++ " [color=\"green\"]") :
               (me ++ " -> " ++ f2' ++ " [color=\"red\"]") : dot s3 }
  )
  where
  me = show r
  s1 = s { dotDone = Set.insert r (dotDone s)
         , dot = (me ++ "[label=" ++ show (labelP p) ++ "]") : dot s
         }
  (f1',s2) = toDot f1 s1
  (f2',s3) = toDot f2 s2


