{-# LANGUAGE Safe #-}
{-# LANGUAGE RecordWildCards, NamedFieldPuns, ViewPatterns #-}
module Group where

import Allegro.Graphics
import Allegro.Primitives
import Control.Monad(zipWithM_,guard)
import Data.Maybe(fromMaybe,listToMaybe)
import SeqPtr


groupMargin :: Float
groupMargin = 10

data Dir = Hor | Ver
           deriving (Eq,Show)

otherDir :: Dir -> Dir
otherDir Hor  = Ver
otherDir Ver  = Hor

--------------------------------------------------------------------------------

data Dim a    = Dim { width, height :: Float, thing :: a }
                deriving (Eq,Show)

getDim :: Dir -> Dim a -> Float
getDim Hor = width
getDim Ver = height

updDim :: Dir -> (Float -> Float) -> Dim a -> Dim a
updDim Hor f Dim { .. } = Dim { width  = f width, .. }
updDim Ver f Dim { .. } = Dim { height = f height, .. }

instance Functor Dim where
  fmap f Dim { .. } = Dim { thing = f thing, .. }

--------------------------------------------------------------------------------

class Basic t where
  dimPrim  :: t -> Dim ()
  drawPrim :: Point -> Dim t -> IO ()

data Shape p  = Prim p
              | Many Dir [Dim (Shape p)]
              | Selected (Maybe (Shape p))

instance Functor Shape where
  fmap f sh = case sh of
                Prim p     -> Prim (f p)
                Many d xs  -> Many d $ fmap (fmap (fmap f)) xs
                Selected m -> Selected $ fmap (fmap f) m

foldShape :: (Dir -> p -> a) -> (Dir -> [a] -> a) -> Dir -> Shape p -> a
foldShape ifPrim ifMany = go
  where go d (Prim p)             = ifPrim d p
        go _ (Many d xs)          = ifMany d (map (go d . thing) xs)
        go d (Selected (Just s))  = go d s
        go d (Selected Nothing)   = ifMany d []


prim :: Basic p => p -> Dim (Shape p)
prim p = Dim { thing = Prim p, .. }
  where Dim { .. } = dimPrim p

manySh :: Dir -> [Dim (Shape p)] -> Dim (Shape p)
manySh d ds = Dim { width = ms + w, height = ms + h, thing = Many d ds }
  where
  ws    = map width ds
  hs    = map height ds
  ms    = 2 * groupMargin

  (w,h) =
    case d of
      Hor -> (sum ws, maximum (0 : hs))
      Ver -> (maximum (0 : ws), sum hs)

selected :: Maybe (Dim (Shape p)) -> Dim (Shape p)
selected ms = Dim { thing = Selected s, width = w, height = h }
  where (s,w,h) = case ms of
                    Just ds -> (Just (thing ds), width ds, height ds)
                    Nothing -> (Nothing, groupMargin, groupMargin)

shDraw :: Basic p => Point -> Dim (Shape p) -> IO ()
shDraw (x,y) me@Dim { thing = mySh, .. } =
  case mySh of
    Prim p -> drawPrim (x,y) $ fmap (\_ -> p) me

    Many d as -> zipWithM_ shDraw positions updatedAs
       where
       updatedAs = map expand as
       margins   = 2 * groupMargin

       expand s   = updDim (otherDir d) (\_ -> getDim (otherDir d) me - margins)
                  $ if consider s then updDim d (pad +) s else s
       pad        = (getDim d me - margins - sum (map (getDim d) as))
                  / flexNumber
       flexNumber = fromIntegral $ length $ filter consider as

       consider s = case thing s of
                      Selected Nothing -> False
                      _                -> True

       positions         = scanl nextPos (x+groupMargin,y+groupMargin) updatedAs
       nextPos (px,py) s = case d of
                             Hor -> (px + getDim Hor s, py)
                             Ver -> (px, py + getDim Ver s)

    Selected s ->
      do drawShape Rectangle
                     { rectTopLeft     = (x,y)
                     , rectBottomRight = (x + width, y + height)
                     , rectCurved      = Nothing
                     } (Color 0 0 0.5 1) Filled
         case s of
           Just th -> shDraw (x,y) $ fmap (\_ -> th) me
           Nothing -> return ()



--------------------------------------------------------------------------------

data UI p = UI { uiPtr    :: SeqPtr (Dim (Shape p))
               , uiDir    :: Dir
               , uiOpened :: [ ([Dim (Shape p)], [Dim (Shape p)]) ]
               }



uiAtTop :: UI p -> Bool
uiAtTop UI { .. } = null uiOpened

uiAlone :: UI p -> Bool
uiAlone UI { .. } = null (seqBefore uiPtr) && null (seqAfter uiPtr)

uiCurShape :: UI p -> Maybe (Shape p)
uiCurShape UI { .. } = fmap thing (seqCur uiPtr)




--------------------------------------------------------------------------------
-- Traversals

instance Functor UI where
  fmap f UI { .. } = UI { uiPtr     = fmap dfmap uiPtr
                        , uiOpened  = map (fmap2 (map dfmap)) uiOpened
                        , .. }
    where
    dfmap = fmap (fmap f)
    fmap2 g (x,y) = (g x, g y)

foldUI :: (Dir -> p -> a) -> (Dir -> [a] -> a) -> UI p -> a
foldUI ifPrim ifMany ui = foldShape ifPrim ifMany (uiDir ui)
                        $ thing $ uiToShape ui




seqPtrToShape :: Dir -> SeqPtr (Dim (Shape p)) -> Dim (Shape p)
seqPtrToShape d SeqPtr { .. } =
  manySh d (jnR seqBefore (selected seqCur : seqAfter))

uiToShape :: UI p -> Dim (Shape p)
uiToShape UI { .. } =
  parens (otherDir uiDir) (seqPtrToShape uiDir uiPtr) uiOpened
  where
  parens _ inner [] = inner
  parens d inner ((ls,rs) : more) =
    parens (otherDir d) (manySh d (jnR ls (inner : rs))) more



emptyUI :: UI p
emptyUI = UI { uiPtr = seqEmpty, uiDir = Ver, uiOpened = [] }


uiMoveSel :: SeqDir -> UI p -> UI p
uiMoveSel side ui@UI { .. } =
  case seqCur uiPtr of
    Nothing -> case side of
                  Prev -> uiMove LT ui
                  Next -> uiMove GT ui
    Just dsh ->
      fromMaybe ui $
      do (a,new) <- seqExtract side uiPtr
         let els = case thing dsh of
                     Many _ [] -> a
                     Many d ds | d == uiDir ->
                       manySh uiDir $ case side of
                                        Prev -> a : ds
                                        Next -> ds ++ [a]
                     _ -> manySh uiDir $ case side of
                                           Prev -> [a,dsh]
                                           Next -> [dsh,a]

         return UI { uiPtr = seqSet (Just els) new, .. }


uiStartSeq :: Ordering -> UI p -> UI p
uiStartSeq side ui@UI { .. } =
  case seqCur uiPtr of
    Just sh | not (uiAlone ui) ->
      let d'  = otherDir uiDir
          els = case thing sh of
                  Selected {} -> []
                  Prim {}     -> [sh]
                  Many d xs   -> if d == d' then xs else [sh]
      in UI { uiDir    = d'
            , uiPtr    = newSeq side [] [] els
            , uiOpened = (seqBefore uiPtr, seqAfter uiPtr) : uiOpened
            }
    _ -> uiClose side ui

uiClose :: Ordering -> UI p -> UI p
uiClose side UI { .. } =
  case seqCur uiPtr of
    Just sh | Many d xs <- thing sh, d == uiDir ->
         UI { uiPtr = newSeq side (seqBefore uiPtr) (seqAfter uiPtr) xs
            , .. }
    _ -> UI { uiDir    = d'
            , uiPtr    = newSeq side before after els
            , uiOpened = drop 1 uiOpened
            }
  where
  d'  = otherDir uiDir

  (before,after) = fromMaybe ([],[]) (listToMaybe uiOpened)

  els = case seqToList uiPtr of
          []   -> []
          [sh] -> case thing sh of
                    Many d ds | d == d' -> ds
                    _                   -> [sh]
          xs   -> [ manySh uiDir xs ]



uiWithPrim :: Basic p => (p -> p) -> UI p -> UI p
uiWithPrim f UI { .. } =
  case uiCurShape UI { .. } of
    Just (Prim p) ->
      let newP = Just $ prim $ f p
      in UI { uiPtr = seqSet newP uiPtr, .. }
    _   -> UI { .. }

uiOnPrim :: (p -> a) -> a -> UI p -> a
uiOnPrim f a ui =
  case uiCurShape ui of
    Just (Prim p) -> f p
    _             -> a

uiWithCur :: (SeqPtr (Dim (Shape p)) -> SeqPtr (Dim (Shape p))) -> UI p -> UI p
uiWithCur f UI { .. } = UI { uiPtr = f uiPtr, .. }

uiMove :: Ordering -> UI p -> UI p
uiMove side UI { .. } = UI { uiPtr = newPtr, .. }
 where
  newPtr =
    case seqCur uiPtr of
      Just sh | Many d xs <- thing sh, d == uiDir ->
        newSeq side (seqBefore uiPtr) (seqAfter uiPtr) xs
      _ -> case side of
             LT -> seqMove Prev uiPtr
             EQ -> uiPtr
             GT -> seqMove Next uiPtr

uiSet :: Basic p => Maybe p -> UI p -> UI p
uiSet a = uiWithCur $ seqSet $ fmap prim a



