{-# LANGUAGE Safe #-}
{-# LANGUAGE RecordWildCards, NamedFieldPuns, ViewPatterns #-}
module Group where

import Allegro.Graphics
import Allegro.Primitives
import Control.Monad(zipWithM_)
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
manySh d ds0 = Dim { width = ms + w, height = ms + h, thing = Many d ds }
  where
  ds    = concatMap flat ds0
  ws    = map width ds
  hs    = map height ds
  ms    = 2 * groupMargin

  (w,h) =
    case d of
      Hor -> (sum ws, maximum (0 : hs))
      Ver -> (maximum (0 : ws), sum hs)

  flat p =
    case thing p of
      Prim {} -> [p]
      Many d' ds' | d == d'    -> ds'
                  | otherwise  -> [p]
      Selected {} -> [p]  -- ?



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


data UI p = UI { uiPtr    :: SeqPtr (Dim (Shape p))
               , uiDir    :: Dir
               , uiOpened :: [ ([Dim (Shape p)], [Dim (Shape p)]) ]
               }

instance Functor UI where
  fmap f UI { .. } = UI { uiPtr     = fmap dfmap uiPtr
                        , uiOpened  = map (fmap2 (map dfmap)) uiOpened
                        , .. }
    where
    dfmap = fmap (fmap f)
    fmap2 g (x,y) = (g x, g y)


emptyUI :: UI p
emptyUI = UI { uiPtr = seqEmpty, uiDir = Ver, uiOpened = [] }

uiAtTop :: UI p -> Bool
uiAtTop UI { .. } = null uiOpened

uiNorm UI { .. } =
  case seqCur uiPtr of
    Just dsh ->
      case thing dsh of
        Many _ []  -> UI { uiPtr = seqSet Nothing uiPtr, .. }
        Many _ [d] -> UI { uiPtr = seqSet (Just d) uiPtr, .. }
        _          -> UI { .. }
    Nothing -> UI { .. }

{-
uiMoveSel :: SeqDir -> UI p -> UI p
uiMoveSel d UI { .. } ->
  case seqCur uiPtr of
    Nothing -> uiMove d UI { .. }
    Just dsh ->
      case 
      -}


uiStartSeq :: Ordering -> UI p -> UI p
uiStartSeq side ui =
  case uiNorm ui of
    UI { .. } ->
      case seqCur uiPtr of
        Nothing -> UI { .. }
        Just dsh ->
          let d' = otherDir uiDir
              doit xs o = UI { uiDir = d', uiPtr = mk xs, uiOpened = o }
          in
          case thing dsh of
            Selected {} -> UI { .. }

            Many d xs ->
               case (seqBefore uiPtr, seqAfter uiPtr, uiOpened) of
                 ([],[],[]) -> doit (if d' == d then xs else [dsh]) uiOpened
                 ([],[],_)  -> UI { .. }
                 (b,a,o)    -> doit (if d' == d then xs else [dsh]) ((b,a) : o)

            Prim {} ->
               case (seqBefore uiPtr, seqAfter uiPtr, uiOpened) of
                 ([],[],[]) -> doit [dsh] uiOpened
                 ([],[],_)  -> UI { .. }
                 (b,a,o)    -> doit [dsh] ((b,a) : o)
  where
  mk as =
    case (side, as) of
      (LT, xs) ->
        SeqPtr { seqBefore = [], seqCur = Nothing, seqAfter = xs }

      (EQ, x : xs) ->
        SeqPtr { seqBefore = [], seqCur = Just x, seqAfter = xs }

      (GT, xs) ->
        SeqPtr { seqBefore = reverse xs, seqCur = Nothing, seqAfter = [] }

      _ -> seqEmpty




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

uiMove :: SeqDir -> UI p -> UI p
uiMove d = uiWithCur $ seqMove d

uiSet :: Maybe (Dim (Shape p)) -> UI p -> UI p
uiSet a = uiWithCur (seqSet a)

uiCurShape :: UI p -> Maybe (Shape p)
uiCurShape UI { .. } =
  do Dim { thing } <- seqCur uiPtr
     return thing

uiSplit :: Ordering -> UI p -> UI p
uiSplit = uiStartSeq {-pos UI { .. } =
  case seqCur uiPtr of
    Just dshape ->
      case thing dshape of
        _ | ( case (seqBefore uiPtr, seqAfter uiPtr) of
                      ([],[]) -> null uiOpened -- orphans may only be split
                                               -- at the top level
                      _ -> True
                  ) ->
         UI { uiDir    = otherDir uiDir
            , uiOpened = (seqBefore uiPtr, seqAfter uiPtr) : uiOpened
            , uiPtr    = case pos of
                           LT -> SeqPtr { seqBefore = []
                                        , seqCur    = Nothing
                                        , seqAfter  = [dshape]
                                        }
                           EQ -> SeqPtr { seqBefore = []
                                        , seqCur    = Just dshape
                                        , seqAfter  = []
                                        }
                           GT -> SeqPtr { seqBefore = [dshape]
                                        , seqCur    = Nothing
                                        , seqAfter  = []
                                        }
               }

        _ -> uiOpen pos UI { .. }

    _ -> UI { .. }
-}


uiOpen :: Ordering -> UI p -> UI p
uiOpen pos UI { .. } =
  case seqCur uiPtr of
    Nothing -> UI { .. }
    Just a ->
      case thing a of
        Many _ []       -> uiSet Nothing UI { .. }
        Many d (x:xs)
          | d == uiDir  -> flatten x xs
          | otherwise   -> enter x xs
        _ -> UI { .. }
  where
  flatten x xs =
    let SeqPtr { .. } = uiPtr
    in UI { uiPtr = SeqPtr { seqCur = Just x, seqAfter = xs ++ seqAfter, .. }
          , .. }

  enter a as =
    UI { uiPtr    = case pos of
                      LT -> SeqPtr { seqBefore = []
                                   , seqCur    = Nothing
                                   , seqAfter  = a : as
                                   }
                      EQ -> SeqPtr { seqBefore = []
                                   , seqCur = Just a
                                   , seqAfter = as }
                      GT  -> SeqPtr { seqBefore = reverse (a : as)
                                    , seqCur = Nothing
                                    , seqAfter = []
                                    }
       , uiDir    = otherDir uiDir
       , uiOpened = (seqBefore uiPtr,seqAfter uiPtr) : uiOpened
       }

uiClose :: UI p -> UI p
uiClose UI { .. } =
  case seqCur uiPtr of
    Just (thing -> Many _ []) -> UI { uiPtr = seqSet Nothing uiPtr, .. }
    Just (thing -> Many d (x:xs))
      | d == uiDir -> UI { uiPtr = let s = seqSet (Just x) uiPtr
                                   in s { seqAfter = xs ++ seqAfter s }, .. }
    _ ->
       case uiOpened of
         (seqBefore,seqAfter) : others ->
             UI { uiPtr    = SeqPtr { seqCur = new, .. }
                , uiDir    = otherDir uiDir
                , uiOpened = others
                }
         [] -> case els of
                 _ : _ : _ -> UI { uiPtr = SeqPtr { seqBefore = []
                                                  , seqAfter = []
                                                  , seqCur = new }
                                  , uiDir = otherDir uiDir
                                  , uiOpened = []
                                  }
                 _ -> UI { .. }
  where
  els = seqToList uiPtr
  new = case els of
            []  -> Nothing
            [x] -> Just x
            xs  -> Just (manySh uiDir xs)

