{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# OPTIONS_GHC -Wno-missing-methods    #-}

module PowerSerious where

import Control.Comonad
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Ratio

newtype PowS a = PowS { fromPowS :: [a] } deriving Eq

instance Functor PowS where
  fmap f (PowS a) = PowS (fmap f a)

-- instance Comonad PowS where
--   -- duplicate p ps = p <:> ps
--   -- extract ps = PowS (tail $ fromPowS ps) --infix operator for tail 
--   -- extract (PowS [a]) = a
--   -- duplicate a = PowS [a]
  
-- instance ComonadApply PowS where
--   f <@> a = extract f <$> a

infixr 5 <:>
(<:>) :: a -> PowS a -> PowS a
f <:> fs = PowS (f : (fromPowS fs))

splitPS :: PowS a -> (Maybe a, Maybe (PowS a))
splitPS = (,) <$> headPS <*> tailPS where
  headPS ps = listToMaybe $ fromPowS ps
  tailPS ps = case fromPowS ps of
              [] -> Nothing
              x -> Just (PowS (tail x))

-- splitPS :: PowS a -> (a, PowS a)
-- splitPS ps = (head u, PowS (tail u)) where u = fromPowS ps
  
pattern f :<>: ft <- (Just f, Just ft)
pattern Empty     <- (_, Nothing)

instance {-# OVERLAPPING #-} Show (PowS Rational) where
  show ps = intercalate " + " $ showPS $ fromPowS ps where
    showPS ps = map (\p -> if denominator p == 1
                           then show $ numerator p
                           else (show $ numerator p) ++ "/" ++ (show $ denominator p))
                ps

instance {-# OVERLAPPING #-} Show a => Show (PowS [a]) where
  show ps = substitute " + " "," $ substitute "" "]" $ substitute "" "[" $ substitute "\n" "],[" $
    init $ tail $ show $ fromPowS ps where
    substitute x y z = intercalate x $ splitOn y $ z
                
instance {-# OVERLAPPING #-} Show a => Show (PowS (PowS a)) where
  show ps = substitute " + " "," $ substitute "\n" "],[" $ init $ tail $
            substitute "" " % 1" $ init $ tail $ show $ (fromPowS <$> fromPowS ps) where
    substitute x y z = intercalate x $ splitOn y $ z

instance (Num a, Eq a) => Num (PowS a) where
  fromInteger c = PowS [fromInteger c]

  negate fs = negate <$> fs

  fs + gs = case (splitPS fs, splitPS gs) of
    (f:<>:ft, g:<>:gt) -> f+g <:> ft+gt
    (_, Empty)         -> fs
    (Empty, _)         -> gs

  fs * gs = case (splitPS fs, splitPS gs) of
    (0:<>:ft, _)       -> 0 <:> ft * gs   -- caveat: 0*diverge = 0
    (_, 0:<>:gt)       -> 0 <:> fs * gt 
    (f:<>:ft, g:<>:gt) -> f*g <:> ft * gs + PowS [f] * gt
    (_,_)              -> PowS []

instance (Fractional a, Eq a) => Fractional (PowS a) where 
  fromRational c = PowS [fromRational c]

  fs / gs = case (splitPS fs, splitPS gs) of
    (0:<>:ft, 0:<>:gt) -> ft / gt
    (0:<>:ft, _)       -> 0 <:> ft / gs 
    (f:<>:ft, g:<>:gt) -> f/g <:> (ft - PowS [f/g] * gt) / gs
    (Empty, 0:<>:gt)   -> PowS [] / gt
    (Empty, g:<>:gt)   -> PowS []
    (_,_)              -> error "improper power series division"
  
infixr 9 #
(#) :: (Eq a, Num a) => PowS a -> PowS a -> PowS a
fs # gs = case (splitPS fs, splitPS gs) of 
  (f:<>:ft, 0:<>:gt) -> f <:> gt * ft # gs
  (f:<>:ft, g:<>:gt) -> PowS [f] + gs * ft # gs  -- ft must be polynomial
  (Empty, _)         -> PowS []
  (f:<>:_, Empty)    -> PowS [f]

revert :: (Eq a, Fractional a) => PowS a -> PowS a 
revert fs = case fromPowS fs of
  (_:0:_) -> error "revert f where f'(0)==0" 
  (0:ft)  -> rs where rs = 0 <:> PowS [1] / PowS ft # rs
  [f,f']  -> PowS [-f/f',1/f']
  _       -> error "revert f where f(0)/=0"

int :: (Fractional a, Enum a) => PowS a -> PowS a 
int fs = PowS (0 : zipWith (/) (fromPowS fs) [1..])

diff :: (Num a, Enum a) => PowS a -> PowS a
diff fs = PowS $ zipWith (*) (tail $ fromPowS fs) [1..]

-- replaces `take` from prelude
takeS :: Int -> PowS a -> PowS a
takeS i s = PowS (take i $ fromPowS s)

-- like `takeS` but returns a vanilla list
takeList :: Int -> PowS a -> [a]
takeList i s = take i $ fromPowS s
