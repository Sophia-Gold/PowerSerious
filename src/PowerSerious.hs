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

wu :: [a] -> (PowS' a -> PowS' a -> PowS' a) -> [a] -> [a]
wu fs op gs = fromPowS $ op (PowS' fs) (PowS' gs)

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
  
  negate fs = PowS' (map negate $ fromPowS fs)

  fs + gs = PowS' $ case (fromPowS fs, fromPowS gs) of
    (f:ft, g:gt) -> f+g : wu ft (+) gt
    (fs, [])     -> fs
    ([], gs)     -> gs

  fs * gs = PowS' $ case (fromPowS fs, fromPowS gs) of
    (0:ft, gs)   -> 0 : wu ft (*) gs   -- caveat: 0*diverge = 0
    (fs, 0:gt)   -> 0 : wu fs (*) gt 
    (f:ft, gs@(g:gt)) -> f*g : wu (wu ft (*) gs) (+) (wu [f] (*) gt)
    (_,_)        -> []
    
instance (Fractional a, Eq a) => Fractional (PowS' a) where 
  fromRational c = PowS' [fromRational c]

  fs / gs = PowS' $ case (fromPowS fs, fromPowS gs) of
    (0:ft, 0:gt) -> wu ft (/) gt
    (0:ft, gs)   -> 0 : wu ft (/) gs 
    (f:ft, gs@(g:gt)) -> f/g : wu (wu ft (-) $ wu [f/g] (*) gt) (/) gs
    ([], 0:gt)   -> wu [] (/) gt
    ([], g:gt)   -> []
    (_,_)        -> error "improper power series division"
  
infixr 9 #
(#) :: (Eq a, Num a) => PowS' a -> PowS' a -> PowS' a 
fs # gs = PowS' $ case (fromPowS fs, fromPowS gs) of 
  (f:ft, gs@(0:gt)) -> f : (wu gt (*) $ wu ft (#) gs) 
  (f:ft, gs@(g:gt)) -> wu [f] (+) $ wu gs (*) $ wu ft (#) gs  -- ft must be polynomial
  ([], _)      -> []
  (f:_, [])    -> [f]

revert :: (Eq a, Fractional a) => PowS' a -> PowS' a 
revert fs = PowS' $ case fromPowS fs of
  (_:0:_) -> error "revert f where f'(0)==0" 
  (0:ft)  -> rs where rs = 0 : (wu [1] (/) $ wu ft (#) rs)
  [f,f']  -> [-f/f',1/f']
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
