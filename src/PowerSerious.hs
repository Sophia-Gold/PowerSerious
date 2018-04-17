{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wno-missing-methods    #-}

module PowerSerious where

import Control.Comonad
import Data.List
import Data.List.Split
import Data.Ratio

newtype PowS' a = PowS' { fromPowS :: [a] } deriving Eq

instance Functor PowS' where
  fmap f (PowS' a) = PowS' (fmap f a)

instance Comonad PowS' where
  extract (PowS' [a]) = a
  duplicate a = PowS' [a]

instance ComonadApply PowS' where 
  f <@> a = extract f <$> a

infixr 5 <:>
(<:>) :: a -> PowS' a -> PowS' a
f <:> fs = PowS' (f : (fromPowS fs))

wu :: [a] -> (a -> a -> a) -> [a] -> PowS' a
wu fs op gs = liftW2 op (PowS' fs) (PowS' gs)

type PowS = PowS' Rational
type PowS2 = PowS' (PowS' Rational)

instance {-# OVERLAPPING #-} Show (PowS' Rational) where
  show ps = intercalate " + " $ showPS $ fromPowS ps where
    showPS ps = map (\p -> if denominator p == 1
                           then show $ numerator p
                           else (show $ numerator p) ++ "/" ++ (show $ denominator p))
                ps
                
instance {-# OVERLAPPING #-} Show a => Show (PowS' (PowS' a)) where
  show ps = substitute " + " "," $ substitute "\n" "],[" $ init $ tail $
            substitute "" " % 1" $ init $ tail $ show $ (fromPowS <$> fromPowS ps) where
    substitute x y z = intercalate x $ splitOn y $ z

instance (Num a, Eq a) => Num (PowS' a) where
  fromInteger c = PowS' [fromInteger c]

  negate fs = negate <$> fs

  fs + gs = case (fromPowS fs, fromPowS gs) of 
    (f:ft, g:gt) -> f+g <:> wu ft (+) gt
    (fs, [])     -> PowS' fs
    ([], gs)     -> PowS' gs

  fs * gs = case (fromPowS fs, fromPowS gs) of
    (0:ft, gs)        -> 0 <:> wu ft (*) gs   -- caveat: 0*diverge = 0
    (fs, 0:gt)        -> 0 <:> wu fs (*) gt 
    (f:ft, gs@(g:gt)) -> f*g <:> wu ft (*) gs + wu [f] (*) gt
    (_,_)             -> PowS' []
    
instance (Fractional a, Eq a) => Fractional (PowS' a) where 
  fromRational c = PowS' [fromRational c]

  fs / gs = case (fromPowS fs, fromPowS gs) of
    (0:ft, 0:gt) -> wu ft (/) gt
    (0:ft, gs)   -> 0 <:> wu ft (/) gs 
    (f:ft, g:gt) -> f/g <:> (PowS' ft - wu [f/g] (*) gt) / gs
    ([], 0:gt)   -> wu [] (/) gt
    ([], g:gt)   -> PowS' []
    (_,_)        -> error "improper power series division"
  
infixr 9 #
(#) :: (Eq a, Num a) => PowS' a -> PowS' a -> PowS' a
fs # gs = case (fromPowS fs, fromPowS gs) of 
  (f:ft, 0:gt) -> f <:> PowS' gt * PowS' ft # gs
  (f:ft, g:gt) -> PowS' [f] + gs * PowS' ft # gs  -- ft must be polynomial
  ([], _)           -> PowS' []
  (f:_, [])         -> PowS' [f]

revert :: (Eq a, Fractional a) => PowS' a -> PowS' a 
revert fs = case fromPowS fs of
  (_:0:_) -> error "revert f where f'(0)==0" 
  (0:ft)  -> rs where rs = 0 <:> PowS' [1] / PowS' ft # rs
  [f,f']  -> PowS' [-f/f',1/f']
  _       -> error "revert f where f(0)/=0"

int :: (Fractional a, Enum a) => PowS' a -> PowS' a 
int fs = PowS' (0 : zipWith (/) (fromPowS fs) [1..])

diff :: (Num a, Enum a) => PowS' a -> PowS' a
diff fs = PowS' $ zipWith (*) (tail $ fromPowS fs) [1..]

-- replaces `take` from prelude
takeS :: Int -> PowS' a -> PowS' a
takeS i s = PowS' (take i $ fromPowS s)

-- like `takeS` but returns a vanilla list
takeList :: Int -> PowS' a -> [a]
takeList i s = take i $ fromPowS s
