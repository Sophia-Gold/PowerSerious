{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# OPTIONS_GHC -Wno-missing-methods    #-}

module PowerSerious where

import Data.List
import Data.List.Split
import Data.Ratio

default (Integer, Rational, Double)
infixr 9 #

newtype PowS a = PowS [a] deriving Eq
-- newtype PowS a = PowS { fromPowS :: [a] } deriving Eq

instance {-# OVERLAPPING #-} Show a => Show (PowS (Ratio a)) where 
  show (PowS ps) = intercalate " + " $ showPS ps where
    showPS ps = map (\p -> (show $ numerator p) ++ "/" ++ (show $ denominator p)) ps

instance Show a => Show (PowS a) where 
  show (PowS ps) = intercalate " + " $ splitOn "," (init $ tail $ show ps)

instance (Num a, Eq a) => Num (PowS a) where
   fromInteger c = fromInteger c
   
   negate (PowS fs) = PowS (map negate fs)

   PowS (f:ft) + PowS (g:gt) = PowS (f+g:t) where PowS t = PowS ft + PowS gt
   fs + PowS [] = fs
   PowS [] + gs = gs

   PowS (0:ft) * gs = PowS (0:t) where PowS t = PowS ft * gs   -- caveat: 0*diverge = 0
   PowS fs * PowS (0:gt) = PowS (0:t) where PowS t =  PowS fs * PowS gt
   PowS (f:ft) * PowS gs@(g:gt) = PowS (f*g:t) where PowS t =  PowS ft * PowS gs + PowS [f] * PowS gt
   _ * _ = PowS []

instance (Fractional a, Eq a) => Fractional (PowS a) where
   fromRational c = fromRational c

   PowS (0:ft) / PowS gs@(0:gt) = PowS ft / PowS gt
   PowS (0:ft) / gs = PowS (0:t) where PowS t = PowS ft / gs
   PowS (f:ft) / PowS gs@(g:gt) = PowS (f/g:t) where PowS t = PowS ft - PowS [f/g] * PowS gt / PowS gs
   PowS [] / PowS (0:gt) = PowS [] / PowS gt
   PowS [] / PowS (g:gt) = PowS []
   _ / _ = error "improper power series division"

(#) :: (Eq a, Num a) => PowS a -> PowS a -> PowS a
PowS (f:ft) # gs@(PowS (0:gt)) = PowS (f : gt) * PowS ft # gs
PowS (f:ft) # gs@(PowS (g:gt)) = PowS [f] + gs * PowS ft # gs -- ft must be polynomial
PowS [] # _ = PowS []
PowS (f:_) # PowS [] = PowS [f]

revert :: (Eq a, Num a, Fractional a) => PowS a -> PowS a
revert (PowS (_:0:_)) = error "revert f where f'(0)==0"
revert (PowS (0:ft)) = rs where rs = PowS ([0, 1]) / PowS ft # rs
revert (PowS [f,f']) = PowS [-f/f',1/f']
revert _ = error "revert f where f(0)/=0"

int :: (Fractional a, Num (PowS a), Fractional (PowS a)) => PowS a -> PowS a
int (PowS fs) = PowS (0 : zipWith (/) fs (map fromInteger [1..]))
diff :: (Num a, Num (PowS a), Fractional (PowS a)) => PowS a -> PowS a
diff (PowS (_:ft)) = PowS $ zipWith (*) ft (map fromInteger [1..])

tans :: (Eq a, Fractional a, Num (PowS a), Fractional (PowS a)) => PowS a
tans = revert $ int 1 / PowS [1,0,1]

sins :: (Fractional a, Num (PowS a), Fractional (PowS a)) => PowS a
sins = int coss

coss :: (Fractional a, Num (PowS a), Fractional (PowS a)) => PowS a
coss = 1 - int sins

pascal :: (Eq a, Fractional a, Num (PowS a), Fractional (PowS a)) => PowS (PowS a)
pascal = 1 / PowS [1, - PowS [1,1]]

takeS :: (Num (PowS a), Fractional (PowS a)) => Int -> PowS a -> PowS a
takeS i (PowS s) = PowS (take i s)
