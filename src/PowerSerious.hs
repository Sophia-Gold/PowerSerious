{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}

module PowerSerious where

default (Integer,Rational,Double)
infixr 9 #

newtype PowS a = PowS a deriving Eq
-- newtype PowS a = PowS { fromPowS :: a } deriving Eq

instance (Num a, Eq a) => Num (PowS [a]) where
   fromInteger c = PowS [fromInteger c]

   negate (PowS fs) = PowS (map negate fs)

   PowS (f:ft) + PowS (g:gt) = PowS (f+g:t) where PowS t = PowS ft + PowS gt
   fs + PowS [] = fs
   PowS [] + gs = gs

   PowS (0:ft) * gs = PowS (0:t) where PowS t =  PowS ft * gs   -- caveat: 0*diverge = 0
   PowS fs * PowS (0:gt) = PowS (0:t) where PowS t =  PowS fs * PowS gt
   PowS (f:ft) * PowS gs@(g:gt) = PowS (f*g:t) where PowS t =  PowS ft * PowS gs + PowS [f] * PowS gt
   _ * _ = PowS []

instance (Fractional a, Eq a) => Fractional (PowS [a]) where
   fromRational c = PowS [fromRational c]

   PowS (0:ft) / PowS gs@(0:gt) = PowS ft / PowS gt
   PowS (0:ft) / gs = PowS (0:t) where PowS t = PowS ft / gs
   PowS (f:ft) / PowS gs@(g:gt) = PowS (f/g:t) where PowS t = PowS ft - PowS [f/g] * PowS gt / PowS gs
   PowS [] / PowS (0:gt) = PowS [] / PowS gt
   PowS [] / PowS (g:gt) = PowS []
   _ / _ = error "improper power series division"

(#) :: (Eq a, Num a, Fractional a, Num (PowS [a]), Fractional (PowS [a])) => [a] -> [a] -> [a]
(f:ft) # gs@(0:gt) = f : gt * ft # gs
(f:ft) # gs@(g:gt) = [f] + gs * ft # gs -- ft must be polynomial
[] # _ = []
(f:_) # [] = [f]

revert :: (Eq a, Num a, Fractional a, Num (PowS [a]), Fractional (PowS [a])) => [a] -> [a]
revert (_:0:_) = error "revert f where f'(0)==0" 
revert (0:ft) = rs where rs = 0 : 1 / ft # rs
revert [f,f'] = [-f/f',1/f']
revert _ = error "revert f where f(0)/=0"

int :: (Num a, Fractional a, Num (PowS [a]), Fractional (PowS [a])) => [a] -> [a]
int fs = 0 : zipWith (/) fs (map fromInteger [1..])

diff :: (Num a, Fractional a, Num (PowS [a]), Fractional (PowS [a])) => [a] -> [a]
diff (_:ft) = zipWith (*) ft (map fromInteger [1..])

tans = revert(int(1/(1:0:1)))

sins :: (Num a, Fractional a, Num (PowS [a]), Fractional (PowS [a])) => [a]
sins = int coss

coss :: (Num a, Fractional a, Num (PowS [a]), Fractional (PowS [a])) => [a]
coss = 1 - int sins

pascal :: (Num a, Fractional a, Num (PowS [a]), Fractional (PowS [a])) => [[a]]
pascal = 1/[1, -[1,1]]
