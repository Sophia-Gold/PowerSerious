{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wno-missing-methods    #-}

module PowerSerious where

import Data.List
import Data.List.Split
import Data.Ratio

newtype PowS' a = PowS' { fromPowS :: [a] } deriving Eq

type PowS = PowS' Rational
type PowS2 = PowS' (PowS' Rational)

instance {-# OVERLAPPING #-} Show (PowS' Rational) where
  show ps = intercalate " + " $ showPS $ fromPowS ps where
    showPS ps = map (\p -> (show $ numerator p) ++ "/" ++ (show $ denominator p)) ps

instance {-# OVERLAPPING #-} Show a => Show (PowS' (PowS' a)) where
  show ps = intercalate "" $ splitOn " % 1" $
            intercalate " + " $ splitOn "," $ init $ tail $
            intercalate "\n" $ splitOn "],[" (init $ tail $ show $ (fromPowS <$> fromPowS ps))

instance (Num a, Eq a) => Num (PowS' a) where
  fromInteger c = PowS' [fromInteger c]
  
  negate fs = PowS' (map negate $ fromPowS fs)

  fs + gs = case (fromPowS fs, fromPowS gs) of
    (f:ft, g:gt) -> PowS' (f+g:t) where  PowS' t = PowS' ft + PowS' gt
    (fs, [])     -> PowS' fs
    ([], gs)     -> PowS' gs

  fs * gs = case (fromPowS fs, fromPowS gs) of
    (0:ft, gs)   -> PowS' (0:t) where PowS' t = PowS' ft * PowS' gs   -- caveat: 0*diverge = 0
    (fs, 0:gt)   -> PowS' (0:t) where PowS' t =  PowS' fs * PowS' gt
    (f:ft, g:gt) -> PowS' (f*g:t) where PowS' t =  PowS' ft * gs + PowS' [f] * PowS' gt
    (_,_)        ->  PowS' []
    
instance (Fractional a, Eq a) => Fractional (PowS' a) where
  fromRational c = PowS' [fromRational c]

  fs / gs = case (fromPowS fs, fromPowS gs) of
    (0:ft, 0:gt) -> PowS' ft / PowS' gt
    (0:ft, gs)   -> PowS' (0:t) where PowS' t = PowS' ft / PowS' gs
    (f:ft, g:gt) -> PowS' (f/g:t) where PowS' t = PowS' ft - PowS' [f/g] * PowS' gt / gs
    ([], 0:gt)   -> PowS' [] / PowS' gt
    ([], g:gt)   -> PowS' []
    (_,_)        -> error "improper power series division"
  
infixr 9 #
(#) :: (Eq a, Num a) => PowS' a -> PowS' a -> PowS' a
fs # gs = case (fromPowS fs, fromPowS gs) of
  (f:ft, gs@(0:gt)) -> PowS' (f:gt) * PowS' ft # PowS' gs
  (f:ft, gs@(g:gt)) -> PowS' [f] + PowS' gs * PowS' ft # PowS' gs  -- ft must be polynomial
  ([], _)           -> PowS' []
  (f:_, [])         -> PowS' [f]

revert :: (Eq a, Fractional a) => PowS' a -> PowS' a
revert fs = case fromPowS fs of
  (_:0:_) -> error "revert f where f'(0)==0"
  -- (0:ft)  -> PowS' rs where rs =  0 : fromPowS (1 / PowS' ft # PowS' rs)
  (0:ft)  -> PowS' (0:rs) where PowS' rs = 1 / PowS' ft # PowS' rs
  [f,f']  -> PowS' [-f/f',1/f']
  _       -> error "revert f where f(0)/=0"

int :: (Fractional a) => PowS' a -> PowS' a
int fs = PowS' (0 : zipWith (/) (fromPowS fs) (map fromInteger [1..]))

diff :: (Num a) => PowS' a -> PowS' a
diff fs = PowS' $ zipWith (*) (tail $ fromPowS fs) (map fromInteger [1..])

-- replaces `take` from prelude
takeS :: Int -> PowS' a -> PowS' a
takeS i s = PowS' (take i $ fromPowS s)

-- like `takeS` but returns a vanilla list
takeList :: Int -> PowS' a -> [a]
takeList i s = take i $ fromPowS s
