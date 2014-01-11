module BDD where

import Data.Map ( Map )
import qualified Data.Map as Map

type Ref  = Int

data F p  = Bool Bool | Ref !(If p) !Ref

data If p = If p (F p) (F p)
            deriving (Eq,Ord)

data H p  = H { hMap :: !(Map (If p) Ref), hNext :: !Ref}

type HF p = H p -> (F p, H p)

instance Ord p => Eq (F p) where
  x == y  = compare x y == EQ

instance Ord p => Ord (F p) where
  compare (Bool x) (Bool y)   = compare x y
  compare (Bool _) _          = LT
  compare (Ref _ x) (Ref _ y) = compare x y
  compare (Ref _ _) (Bool _)  = GT


mkIf :: Ord p => If p -> HF p
mkIf (If _ f1 f2) h | f1 == f2  = (f1,h)
mkIf f h = (Ref f r, h1)
  where
  next   = hNext h
  (r,h1) = case Map.insertLookupWithKey (\_ _ _ -> next) f next (hMap h) of
             (Just r1, _) -> (r1, h)
             (Nothing, m) -> (next, H { hMap = m, hNext = next + 1 })

simpP :: Ord p => p -> Bool -> F p -> F p
simpP p v (Ref (If q f1 f2) _)
  | p == q    = if v then f1 else f2
simpP _ _ f   = f

fITE :: Ord p => F p -> F p -> F p -> HF p
fITE (Bool b) fT fF h = (if b then fT else fF, h)
fITE p fT fF h =
  let v        = minimum [ x | Ref (If x _ _) _ <- [p, fT, fF] ]
      (fT',h1) = fITE (simpP v True  p) (simpP v True  fT) (simpP v True  fF) h
      (fF',h2) = fITE (simpP v False p) (simpP v False fT) (simpP v False fF) h1
  in mkIf (If v fT' fF') h2

fVar :: Ord p => p -> HF p
fVar p = mkIf (If p (Bool True) (Bool False))

fNot :: Ord p => HF p -> HF p
fNot = un $ \p -> fITE p (Bool False) (Bool True)

fAnd :: Ord p => HF p -> HF p -> HF p
fAnd = bin $ \p q -> fITE p q (Bool False)

fOr :: Ord p => HF p -> HF p -> HF p
fOr = bin $ \p q -> fITE p (Bool True) q


infixr 5 `fImplies`

fImplies :: Ord p => HF p -> HF p -> HF p
fImplies = bin $ \p q -> fITE p q (Bool True)

un :: (F p -> HF p) -> HF p -> HF p
un mk f h =
  let (f',h1) = f h
  in mk f' h1

bin :: (F p -> F p -> HF p) -> (HF p -> HF p -> HF p)
bin mk f1 f2 h =
  let (f1',h1) = f1 h
      (f2',h2) = f2 h1
  in mk f1' f2' h2

done :: HF p -> F p
done f = fst (f h0)
  where h0 = H { hMap = Map.empty, hNext = 0 }

