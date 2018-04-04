{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE OverlappingInstances       #-}

module PowerSerious where

default (Integer,Rational,Double)
infixr 9 #

newtype Diff a = Diff a deriving (Eq, Ord)

instance Num (Diff [a]) where
   fromInteger (Diff c) = Diff [fromInteger c]

   negate (Diff fs) = Diff (map negate fs)

   Diff (f:ft) + Diff (g:gt) = Diff (f+g : ft+gt)
   Diff fs + Diff [] = Diff fs
   Diff [] + Diff gs = Diff gs

   Diff (0:ft) * Diff gs = Diff (0 : ft*gs)   -- caveat: 0*diverge = 0
   Diff fs * Diff (0:gt) = Diff (0 : fs*gt)
   Diff (f:ft) * Diff gs@(g:gt) = Diff (f*g : ft*gs + [f]*gt)
   _ * _ = Diff []

instance Fractional (Diff [a]) where
   fromRational (Diff c) = Diff [fromRational c]

   Diff (0:ft) / Diff gs@(0:gt) = Diff ft/gt
   Diff (0:ft) / Diff gs@(g:gt) = Diff (0 : ft/gs)
   Diff (f:ft) / Diff gs@(g:gt) = Diff (f/g : (ft-[f/g]*gt)/gs)
   [] / Diff (0:gt) = Diff []/gt
   [] / Diff (g:gt) = []
   _ / _ = error "improper power series division"

(f:ft) # gs@(0:gt) = f : gt*(ft#gs)
(f:ft) # gs@(g:gt) = [f] + gs*(ft#gs) -- ft must be polynomial
[] # _ = []
(f:_) # [] = [f]

revert (_:0:_) = error "revert f where f'(0)==0"
revert (0:ft) = rs where rs = 0 : 1/(ft#rs)
revert [f,f'] = [-f/f',1/f']
revert _ = error "revert f where f(0)/=0"

int fs = 0 : zipWith (/) fs (map fromInteger [1..])

diff (_:ft) = zipWith (*) ft (map fromInteger [1..])

tans = revert(int(1/(1:0:1)))
sins = int coss
coss = 1 - int sins

pascal = 1/[1, -[1,1]]
