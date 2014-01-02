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

import Debug.Trace

main :: IO ()
main =
  allegro $
  do f <- loadFont "resources/font.ttf" 12
     withDisplay FixedWindow 800 600 $ \d ->
       do setWindowTitle d "Hello"
          q <- createEventQueue
          registerEventSource q =<< installKeyboard

          redraw <- createTimer (1/30)
          registerEventSource q redraw

          nextExample <- createTimer (1/2)
          registerEventSource q nextExample

          startTimer redraw
          startTimer nextExample

          let goMb Nothing   = return ()
              goMb (Just ui) = go ui

              go ui =
               do ev <- waitForEvent q
                  case ev of
                    KeyChar e ->
                      let ch = do c <- evKeyChar e
                                  guard (isPrint c)
                                  return c
                      in goMb $ uiKey f (evKey e) (evKeyMod e) ch ui

                    Time e
                      | evTimer e == redraw ->
                          do drawScreen ui
                             go ui
                      | evTimer e == nextExample ->
                             go (getNextExample ui)

                    _ -> go ui

          go Screen { theFormula  = emptyUI
                    , goodExamples = []
                    , badExamples  = []
                    , theFont      = f
                    }


data Screen = Screen
  { theFormula   :: UI F
  , goodExamples :: [Actor]
  , badExamples  :: [Actor]
  , theFont      :: Font
  }


drawScreen :: Screen -> IO ()
drawScreen Screen { .. } =
  do clearToColor (Color 0 0 0 0)

     mb goodExamples $ \p1 -> withTransformSRT (5,5) 0 (100,100) (drawActor p1)
     drawLine (Line (200,0) (200,200)) white 3
     mb badExamples $ \p2 -> withTransformSRT (5,5) 0 (300,100) (drawActor p2)


     shDraw (10,250) $ uiToShape theFormula

     flipDisplay

  where mb [] _      = return ()
        mb (x : _) f = f x



theStyle :: Font -> Style
theStyle pFont = Style
  { pBorder   = Just 0
  , pPadX     = 50
  , pPadY     = 10
  , pBgColor  = Color 0 0 0 0
  , pColor    = Color 1 1 1 0
  , pFont
  , pCurColor = Color 0 0 1 0
  }


getNextExample :: Screen -> Screen
getNextExample Screen { .. }
  | uiOnPrim tbEditing False theFormula = Screen { .. }
  | otherwise = Screen { goodExamples = drop 1 goodExamples
                       , badExamples  = drop 1 badExamples
                       , theFormula = case drop 1 badExamples of
                                        x : _ -> fmap (setStyle (actorVars x))
                                                              theFormula
                                        _ -> theFormula
                       , ..
                       }


doneEditing :: Screen -> Screen
doneEditing Screen { .. } =
  let f1 = uiWithPrim tbStopEdit theFormula
      p  = toProp f1

      stGood = foldr assert noProps $ p     : basicConstraints
      stBad  = foldr assert noProps $ Not p : basicConstraints

      mbCy [] = []
      mbCy xs = cycle xs
      enum = mbCy . map (actorFromAssign . slnCurrent)
                   . concatMap slnEnumerate . allSolutions

  in Screen { goodExamples = enum stGood
            , badExamples  = enum stBad
            , theFormula   = f1
            , ..
            }




uiKey :: Font -> Key -> KeyMod -> Maybe Char -> Screen -> Maybe Screen
uiKey f k kms mb ui@Screen { .. }
  | uiOnPrim tbEditing False theFormula =
    Just $
    case () of
      _ | k == key_LEFT           -> textBox tbLeft
        | k == key_RIGHT          -> textBox tbRight
        | k == key_DELETE         -> textBox tbDel
        | k == key_BACKSPACE      -> textBox tbBackSp
        | k == key_INSERT         -> textBox tbToggleIns
        | k == key_ENTER
       || k == key_ESCAPE         -> doneEditing ui
        | Just c <- mb, isPrint c -> textBox (tbChar c)
        | otherwise               -> ui

  | Just 'q' <- mb  = Nothing

  | k == key_DELETE = Just $ doneEditing $ withF $ uiSet Nothing

  | otherwise = Just $ withF $ \fo ->
    case () of
    {-
      _ | trace (show kms) $ k == key_LEFT, kms `kmHas` km_SHIFT
        , uiDir fo == Hor -> uiMoveSel Prev fo

        | k == key_RIGHT, kms `kmHas` km_SHIFT
        , uiDir fo == Hor -> uiMoveSel Next fo

        | k == key_UP, kms `kmHas` km_SHIFT
        , uiDir fo == Ver -> trace "SHIFT SEL" $ uiMoveSel Prev fo

        | k == key_DOWN, kms `kmHas` km_SHIFT
        , uiDir fo == Ver -> uiMoveSel Next fo
        -}

      _ | k == key_LEFT  -> if uiDir fo == Hor then uiMove Prev fo else uiSplit LT fo
        | k == key_RIGHT -> if uiDir fo == Hor then uiMove Next fo else uiSplit GT fo
        | k == key_UP    -> if uiDir fo == Ver then uiMove Prev fo else uiSplit LT fo
        | k == key_DOWN  -> if uiDir fo == Ver then uiMove Next fo else uiSplit GT fo

        | k == key_ENTER  ->
            case uiCurShape fo of
              Nothing ->
                uiSet (Just $ prim $ F (theStyle f) (newEditor "") Err) fo
              Just (Prim _)     -> uiWithPrim tbStartEdit fo
              Just (Many {})    -> uiOpen EQ fo
              _                 -> fo

        | k == key_ESCAPE -> uiClose fo
        | otherwise       -> fo

  where
  withF p   = Screen { theFormula = p theFormula, .. }
  textBox p = withF (uiWithPrim p)

