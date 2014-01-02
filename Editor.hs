{-# LANGUAGE Safe #-}
{-# LANGUAGE RecordWildCards #-}
module Editor
  ( Editor
  , editorText
  , newEditor
  , toggleMode
  , editorMove
  , SeqDir(..)
  , editorChar
  , editorDel
  , editorBackSp
  , Style(..)
  , editorDraw
  , editorDim
  ) where

import Allegro.Font
import Allegro.Primitives
import Data.Maybe(fromMaybe,isNothing)
import Control.Monad(when)
import SeqPtr

newtype Editor = Editor { buffer :: SeqPtr Char }

editorText :: Editor -> String
editorText = seqToList . buffer

newEditor :: String -> Editor
newEditor txt = Editor { buffer = seqFromList txt }

toggleMode :: Editor -> Editor
toggleMode e
  | inserting e = withBuffer (seqMove Next) e
  | otherwise   = withBuffer (seqMove Prev) e

editorMove :: SeqDir -> Editor -> Editor
editorMove d = withBuffer (seqMoves [d,d])

editorChar :: Char -> Editor -> Editor
editorChar a e
  | inserting e = withBuffer (seqMove Next . seqSet (Just a)) e
  | otherwise   = withBuffer (seqMoves [Next,Next] . seqSet (Just a)) e

editorBackSp :: Editor -> Editor
editorBackSp e
  | inserting e = withBuffer (seqSet Nothing . seqMove Prev) e
  | otherwise   = withBuffer (seqSet Nothing . seqMoves [Prev,Prev]) e

editorDel :: Editor -> Editor
editorDel e
  | inserting e = withBuffer (seqSet Nothing . seqMove Next) e
  | otherwise   = withBuffer (seqMove Next . seqSet Nothing) e

withBuffer :: (SeqPtr Char -> SeqPtr Char) -> Editor -> Editor
withBuffer f (Editor b) = Editor (f b)

inserting :: Editor -> Bool
inserting = isNothing . seqCur . buffer



--------------------------------------------------------------------------------

data Style = Style
  { pBorder   :: Maybe Float
  , pPadX     :: Float
  , pPadY     :: Float
  , pBgColor  :: Color
  , pColor    :: Color
  , pFont     :: Font
  , pCurColor :: Color
  }

basicWidth :: Bool -> Font -> Editor -> Float
basicWidth withCur font Editor { .. } =
  fromIntegral $ textWidth font $ seqToList $
    case seqCur buffer of
      Just _ -> buffer
      _      -> if withCur then seqSet (Just '_') buffer else buffer

editorDim :: Bool -> Style -> Editor -> (Float,Float)
editorDim withCur Style { .. } e =
  ( w + 2 * pPadX + fromMaybe 0 pBorder
  , h + 2 * pPadY + fromMaybe 0 pBorder
  )
  where
  h = fromIntegral (fontLineHeight pFont)
  w = basicWidth withCur pFont e


editorDraw :: Bool -> Point -> (Float,Float) -> Style -> Editor -> IO ()
editorDraw withCur (x,y) (width,height) Style { .. } Editor { .. } =
  do let r = Rectangle { rectTopLeft     = (x,y)
                       , rectBottomRight = (x + width, y + height)
                       , rectCurved      = Nothing }

     drawShape r pBgColor Filled
     case pBorder of
       Just b  -> drawShape r pColor (Outlined b)
       Nothing -> return ()

     let t1    = reverse (seqBefore buffer)
         w1    = fromIntegral (textWidth pFont t1)
         t2    = seqAfter buffer

     drawText pFont pColor (tX,tY) AlignLeft t1
     curW <- drawCur (seqCur buffer) (tX + w1)
     drawText pFont pColor (tX + w1 + curW, tY) AlignLeft t2

  where
  drawCur :: Maybe Char -> Float -> IO Float
  drawCur mb curX =
    do let cw = fromIntegral $ textWidth pFont
              $ case mb of
                  Just a  -> [a]
                  Nothing -> if withCur then "_" else ""

       when withCur $
         drawShape Rectangle
           { rectTopLeft     = (curX, tY)
           , rectBottomRight = (curX + cw, tY + fh)
           , rectCurved      = Nothing
           } pCurColor Filled

       case mb of
         Just a  -> drawText pFont pColor (curX,tY) AlignLeft [a]
         Nothing -> return ()

       return cw

  fh = fromIntegral (fontLineHeight pFont)
  tX = x + width / 2 - basicWidth withCur pFont Editor { .. } / 2
  tY = y + (height - fh) / 2


