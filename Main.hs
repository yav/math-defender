{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}
import Allegro
import Allegro.Display
import Allegro.EventQueue
import Allegro.Font
import Allegro.Keyboard
import Allegro.Graphics
import Allegro.Primitives
import Allegro.Timer
import Allegro.Transform
import Data.Char(isPrint)
import Control.Monad(guard,unless)

import Data.Integer.SAT
import Group
import Formula
import Editor
import Actor

import Data.Int(Int64)
import Data.Char(isSpace)
import System.Random
import Control.Monad(forM_)

randF :: IO Prop
randF =
  do op <- do k <- randomRIO (0,3)
              return $ [ (:==), (:/=), (:<), (:<=) ] !! k
     t1 <- randomV
     t2 <- do k <- randomIO
              if k then randomK else randomV
     let f = op t1 t2
         b = foldr assert noProps basicConstraints
     case checkSat (assert f b) of
       Nothing -> randF
       Just _ -> case checkSat (assert (Not f) b) of
                   Nothing -> randF
                   _ -> return (op t1 t2)

  where
  randomK = K `fmap` randomRIO (1,5)
  randomV = do k <- randomRIO (0,3)
               return $ [ vw1, vh1, vw1, vh1, vx2, vy2 ] !! k

drawsPerSec   = 30
examplePerSec = 0.5
screenW = 1024
screenH = 768

main :: IO ()
main =
  allegro $
  do f <- loadFont "resources/font.ttf" 12
     withDisplay FixedWindow screenW screenH $ \d ->
       do setWindowTitle d "Hello"
          q <- createEventQueue
          registerEventSource q =<< installKeyboard

          redraw <- createTimer (1/realToFrac drawsPerSec)
          registerEventSource q redraw

          nextExample <- createTimer (1/realToFrac examplePerSec)
          registerEventSource q nextExample

          startTimer redraw
          startTimer nextExample

          form <- randF
          print form

          let goMb _ Nothing   = return ()
              goMb s (Just ui) = go s ui

              go s ui =
               do ev <- waitForEvent q
                  case ev of
                    KeyChar e ->
                      let ch = do c <- evKeyChar e
                                  guard (isPrint c)
                                  return c
                      in goMb s $ uiKey form f (evKey e) (evKeyMod e) ch ui

                    Time e
                      | evTimer e == redraw ->
                          do drawScreen s (evCount e) ui
                             go s ui
                      | evTimer e == nextExample ->
                          do start <- getTimerCount redraw
                             go start (getNextExample ui)

                    _ -> go s ui

          x <- getTimerCount redraw
          go x Screen { theFormula    = emptyUI
                      , goodRejected  = []
                      , badAccepted   = []
                      , visible       = [True,False,False,False]
                      , theFont       = f
                      }


data Screen = Screen
  { theFormula   :: UI F
  , goodRejected :: [Actor]
  , badAccepted  :: [Actor]
  , visible      :: [Bool]
  , theFont      :: Font
  }


drawScreen :: Int64 -> Int64 -> Screen -> IO ()
drawScreen start now Screen { .. } =
  do clearToColor (Color 0 0 0 0)

     let space  = 50
         steps  = drawsPerSec / examplePerSec

         t :: Float
         t = 2 * fromIntegral (now - start) * (space / steps)

         screenW' = fromIntegral screenW
         screenH' = fromIntegral screenH

         lst x = zip [ -1, 1 .. ] . reverse . take (round (x / space))

     forM_ (lst screenW' goodRejected) $ \(x,p) ->
        withTransformSRT (5,5) 0
            ( fromInteger x * space + t
            , (fromIntegral screenH - space) / 2
            ) (drawActor visible p)

     forM_ (lst screenH' badAccepted) $ \(x,p) ->
        withTransformSRT (5,5) 0
            ( (fromIntegral screenW - space) / 2
            , fromInteger x * space + t
            ) (drawActor visible p)


     shDraw (800,500) $ uiToShape theFormula

     flipDisplay

  where mb [] _      = return ()
        mb (x : _) f = f x



theStyle :: Font -> Style
theStyle pFont = Style
  { pBorder   = Just 0
  , pPadX     = 10
  , pPadY     = 10
  , pBgColor  = Color 0 0 0 0
  , pColor    = Color 1 1 1 0
  , pFont
  , pCurColor = Color 0 0 1 0
  }


getNextExample :: Screen -> Screen
getNextExample Screen { .. }
  | uiOnPrim tbEditing False theFormula = Screen { .. }
  | otherwise = Screen { goodRejected = drop 1 goodRejected
                       , badAccepted  = drop 1 badAccepted
                       , ..
                       }


doneEditing :: Prop -> Screen -> Screen
doneEditing tgt Screen { .. } =
  let f1 = uiWithPrim tbStopEdit theFormula
      f2 = uiOnPrim (\(F _ e _) ->
              if all isSpace (editorText e)
                 then uiSet Nothing f1
                 else f1) f1 f1

      p  = toProp f2

      stGood = foldr assert noProps $ (Not p :&& tgt) : basicConstraints
      stBad  = foldr assert noProps $ (p :&& Not tgt) : basicConstraints

      mbCy [] = []
      mbCy xs = cycle xs
      enum = mbCy . map (actorFromAssign . slnCurrent)
                   . concatMap slnEnumerate . allSolutions

  in Screen { goodRejected = enum stGood
            , badAccepted  = enum stBad
            , theFormula   = f2
            , ..
            }

toggle :: Int -> [Bool] -> [Bool]
toggle _ []       = repeat True
toggle 0 (x : xs) = not x : xs
toggle n (x : xs) = x : toggle (n-1) xs


uiKey :: Prop -> Font -> Key -> KeyMod -> Maybe Char -> Screen -> Maybe Screen
uiKey tgt f k kms mb ui@Screen { .. }
  | uiOnPrim tbEditing False theFormula =
    Just $
    case () of
      _ | k == key_LEFT           -> textBox tbLeft
        | k == key_RIGHT          -> textBox tbRight
        | k == key_DELETE         -> textBox tbDel
        | k == key_BACKSPACE      -> textBox tbBackSp
        | k == key_INSERT         -> textBox tbToggleIns
        | k == key_ENTER
       || k == key_ESCAPE         -> doneEditing tgt ui
        | Just c <- mb, isPrint c -> textBox (tbChar c)
        | otherwise               -> ui

  | Just 'q' <- mb  = Nothing

  | Just 'r' <- mb = Just Screen { visible = toggle 0 visible, .. }
  | Just 'y' <- mb = Just Screen { visible = toggle 1 visible, .. }
  | Just 'g' <- mb = Just Screen { visible = toggle 2 visible, .. }
  | Just 'c' <- mb = Just Screen { visible = toggle 3 visible, .. }


  | k == key_DELETE = Just $ doneEditing tgt $ withF $ uiSet Nothing

  | otherwise = Just $ withF $ \fo ->
    case () of

      _ | k == key_LEFT, kms `kmHas` km_SHIFT
        , uiDir fo == Hor -> uiMoveSel Prev fo

        | k == key_RIGHT, kms `kmHas` km_SHIFT
        , uiDir fo == Hor -> uiMoveSel Next fo

        | k == key_UP, kms `kmHas` km_SHIFT
        , uiDir fo == Ver -> uiMoveSel Prev fo

        | k == key_DOWN, kms `kmHas` km_SHIFT
        , uiDir fo == Ver -> uiMoveSel Next fo

      _ | k == key_LEFT  ->
            if uiDir fo == Hor then uiMove LT fo else uiStartSeq LT fo
        | k == key_RIGHT ->
            if uiDir fo == Hor then uiMove GT fo else uiStartSeq GT fo
        | k == key_UP ->
            if uiDir fo == Ver then uiMove LT fo else uiStartSeq LT fo
        | k == key_DOWN ->
            if uiDir fo == Ver then uiMove GT fo else uiStartSeq GT fo

        | k == key_ENTER  ->
            case uiCurShape fo of
              Nothing ->
                uiSet (Just $ F (theStyle f) (newEditor "") Editing) fo
              Just (Prim _)     -> uiWithPrim tbStartEdit fo
              Just (Many {})    -> uiStartSeq EQ fo
              _                 -> fo

        | k == key_ESCAPE -> uiClose EQ fo
        | otherwise       -> fo

  where
  withF p   = Screen { theFormula = p theFormula, .. }
  textBox p = withF (uiWithPrim p)

