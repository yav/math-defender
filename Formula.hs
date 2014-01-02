{-# LANGUAGE Safe #-}
{-# LANGUAGE RecordWildCards #-}
module Formula where

import Allegro.Graphics(Color(..))
import Data.Integer.SAT
import Text.ParserCombinators.ReadP
import Data.Char(isSpace)
import Data.Maybe(listToMaybe)
import Data.Map(Map)
import qualified Data.Map as Map
import Control.Monad(liftM2,liftM3,guard)

import SeqPtr
import Editor
import Group



data F  = F Style Editor S
data S  = Editing | Err | Ok Prop (Maybe Bool)

instance Basic F where

  dimPrim (F s e p) = Dim { thing = (), .. }
    where (withCur,e') = visibleEditor p e
          (width,height) = editorDim withCur s e'

  drawPrim pos Dim { thing = F s e p, .. } =
      editorDraw withCur pos (width,height) s' e'
    where (withCur,e') = visibleEditor p e
          s' = s {- case p of
                 Ok _ (Just b) -> 
                  if b
                    then s { pBgColor = Color 0 0.3 0 1 }
                    else s { pBorder = Nothing
                           , pColor  = Color 0.4 0 0 1
                           , pBgColor = Color 0 0 0 0
                           }
                 _ -> s -}

visibleEditor :: S -> Editor -> (Bool,Editor)
visibleEditor Editing e = (True, e)
visibleEditor (Ok {}) e = (False, e)
visibleEditor Err e     = (False, newEditor $
                              case editorText e of
                                cs | all isSpace cs -> "(off)"
                                   | otherwise      -> "ERR: " ++ cs)


tbEditing :: F -> Bool
tbEditing (F _ _ Editing) = True
tbEditing _               = False

tbStartEdit :: F -> F
tbStartEdit (F p txt _) = F p txt Editing

tbStopEdit :: F -> F
tbStopEdit (F o e _) = F o e $ case parse (editorText e) of
                                  Nothing -> Err
                                  Just p  -> Ok p Nothing

tbWithEditor :: (Editor -> Editor) -> F -> F
tbWithEditor f (F o e Editing) = F o (f e) Editing
tbWithEditor _ e               = e

tbToggleIns :: F -> F
tbToggleIns = tbWithEditor toggleMode

tbLeft :: F -> F
tbLeft = tbWithEditor $ editorMove Prev

tbRight :: F -> F
tbRight  = tbWithEditor $ editorMove Next

tbChar :: Char -> F -> F
tbChar a = tbWithEditor $ editorChar a

tbDel :: F -> F
tbDel = tbWithEditor editorDel

tbBackSp :: F -> F
tbBackSp = tbWithEditor editorBackSp

--------------------------------------------------------------------------------

toProp :: UI F -> Prop
toProp = foldUI ifOne ifMany
  where
  ifOne _ (F _ _ (Ok p _)) = p
  ifOne _ _              = PTrue

  ifMany Hor []          = PFalse
  ifMany Hor xs          = foldr1 (:||) xs

  ifMany Ver []          = PTrue
  ifMany Ver xs          = foldr1 (:&&) xs


setStyle :: Map Name Integer -> F -> F
setStyle env (F s e (Ok p _)) = F s e (Ok p (evalProp env p))
setStyle _ f = f

evalProp :: Map Name Integer -> Prop -> Maybe Bool
evalProp env prop =
  case prop of
    PTrue     -> Just True
    PFalse    -> Just False
    p1 :|| p2 -> liftM2 (||) (evalProp env p1) (evalProp env p2)
    p1 :&& p2 -> liftM2 (&&) (evalProp env p1) (evalProp env p2)
    Not p     -> fmap not    (evalProp env p)
    e1 :== e2 -> liftM2 (==) (evalExpr env e1) (evalExpr env e2)
    e1 :/= e2 -> liftM2 (/=) (evalExpr env e1) (evalExpr env e2)
    e1 :< e2  -> liftM2 (<)  (evalExpr env e1) (evalExpr env e2)
    e1 :<= e2 -> liftM2 (<=) (evalExpr env e1) (evalExpr env e2)
    e1 :> e2  -> liftM2 (>)  (evalExpr env e1) (evalExpr env e2)
    e1 :>= e2 -> liftM2 (>=) (evalExpr env e1) (evalExpr env e2)

evalExpr :: Map Name Integer -> Expr -> Maybe Integer
evalExpr env expr =
  case expr of
    e1 :+ e2    -> liftM2 (+) (evalExpr env e1) (evalExpr env e2)
    e1 :- e2    -> liftM2 (-) (evalExpr env e1) (evalExpr env e2)
    n :* e      -> fmap (n *) (evalExpr env e)
    Negate e    -> fmap negate (evalExpr env e)
    Var x       -> Map.lookup x env
    K x         -> Just x
    If p e1 e2  -> liftM3 (\x t e -> if x then t else e)
                          (evalProp env p) (evalExpr env e1) (evalExpr env e2)
    Div e x     -> guard (x /= 0) >> fmap (`div` x) (evalExpr env e)
    Mod e x     -> guard (x /= 0) >> fmap (`mod` x) (evalExpr env e)



--------------------------------------------------------------------------------
parse :: String -> Maybe Prop
parse s = listToMaybe [ x | (x,cs) <- readP_to_S pProp s, all isSpace cs ]

pProp :: ReadP Prop
pProp =
  do e1 <- pExpr
     op <- choice [ kw "==" >> return (:==)
                  , kw "!=" >> return (:/=)
                  , kw "<"  >> return (:<)
                  , kw ">"  >> return (:>)
                  , kw "<=" >> return (:<=)
                  , kw ">=" >> return (:>=)
                  ]
     e2 <- pExpr
     return (op e1 e2)

mkAdd :: (Integer -> Integer -> Integer) -> (Expr -> Expr -> Expr)
      -> (Expr -> Expr -> Expr)
mkAdd o _ (K x) (K y) = K (o x y)
mkAdd _ s x y         = s x y

mkMul :: Expr -> Expr -> Maybe Expr
mkMul (K 0) _     = Just $ K 0
mkMul _ (K 0)     = Just $ K 0
mkMul (K x) (K y) = Just $ K $ x * y
mkMul (K x) y     = Just (x :* y)
mkMul x (K y)     = Just (y :* x)
mkMul _ _         = Nothing

mkDiv :: (Integer -> Integer -> Integer) -> (Expr -> Integer -> Expr)
      -> (Expr -> Expr -> Maybe Expr)
mkDiv _ _ _ (K 0)     = Nothing
mkDiv o _ (K x) (K y) = Just $ K $ o x y
mkDiv _ s x (K n)     = Just (s x n)
mkDiv _ _ _ _         = Nothing

mkNeg :: Expr -> Expr
mkNeg (K x) = K (negate x)
mkNeg x     = Negate x

if2 :: (a -> b -> Maybe c) -> Maybe a -> Maybe b -> Maybe c
if2 f (Just x) (Just y) = f x y
if2 _ _ _ = Nothing

pExpr :: ReadP Expr
pExpr = chainl1 pProd $ choice [ kw "+" >> return (mkAdd (+) (:+))
                               , kw "-" >> return (mkAdd (-) (:-)) ]
  where
  mb p = do Just a <- p
            return a

  pProd = mb $ chainl1 (fmap Just pNeg)
             $ choice [ kw "*" >> return (if2 mkMul)
                      , kw "/" >> return (if2 (mkDiv div Div))
                      , kw "%" >> return (if2 (mkDiv mod Mod)) ]

  pNeg  = pAtom <++ (kw "-" >> fmap mkNeg pAtom)

  pAtom = choice [ var "bw" 0
                 , var "bh" 1
                 , var "yw" 2
                 , var "yh" 3
                 , var "dx" 4
                 , var "dy" 5
                 , K `fmap` num
                 , between (kw "(") (kw ")") pExpr
                 ]

  var x y = kw x >> return (Var $ toName y)

  num :: ReadP Integer
  num   = do skipSpaces
             readS_to_P reads

kw :: String -> ReadP ()
kw s = do skipSpaces
          string s >> return ()


