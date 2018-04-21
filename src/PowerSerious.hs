{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# OPTIONS_GHC -Wno-missing-methods    #-}

module PowerSerious where

import Control.Comonad
import Control.Monad
import Data.List
import Data.List.NonEmpty (NonEmpty (..), (<|))
import qualified Data.List.NonEmpty as NE
import Data.List.Split
import Data.Ratio

newtype PowS a = PowS { fromPowS :: NonEmpty a } deriving Eq

instance Functor PowS where
  fmap f (PowS a) = PowS (fmap f a)

infixr 5 <:>
(<:>) :: a -> PowS a -> PowS a
f <:> fs = PowS (f <| fromPowS fs)

splitPS :: PowS a -> (a, Maybe (PowS a))
splitPS ps = (fst u, PowS <$> snd u) where u = NE.uncons $ fromPowS ps
  
pattern f :<>: ft = (f, Just ft)
pattern Empty    <- (_, Nothing)

instance {-# OVERLAPPING #-} Show (PowS Rational) where
  show ps = intercalate " + " $ NE.toList $ showPS $ fromPowS ps where
    showPS ps = fmap (\p -> if denominator p == 1
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
  fromInteger c = PowS (NE.fromList [fromInteger c])

  negate fs = PowS (NE.map negate $ fromPowS fs)

  fs + gs = case (splitPS fs, splitPS gs) of
    (f:<>:ft, g:<>:gt) -> f+g <:> ft+gt
    (_, Empty)         -> fs
    (Empty, _)         -> gs

  fs * gs = case (splitPS fs, splitPS gs) of
    (0:<>:ft, _)       -> 0 <:> ft*gs   -- caveat: 0*diverge = 0
    (_, 0:<>:gt)       -> 0 <:> fs*gt 
    (f:<>:ft, g:<>:gt) -> f*g <:> ft * gs + PowS (f:|[]) * gt
    (_,_)              -> PowS (0:|[])

instance (Fractional a, Eq a) => Fractional (PowS a) where 
  fromRational c = PowS (NE.fromList [fromRational c]) 

  fs / gs = case (splitPS fs, splitPS gs) of
    (0:<>:ft, 0:<>:gt) -> ft / gt
    (0:<>:ft, _)       -> 0 <:> ft / gs
    (f:<>:ft, g:<>:gt) -> f/g <:> (ft - PowS (f/g:|[]) * gt) / gs
    (Empty, 0:<>:gt)   -> PowS (0:|[]) / gt
    (Empty, g:<>:gt)   -> PowS (0:|[])
    (_,_)              -> error "improper power series division"
  
infixr 9 #
(#) :: (Eq a, Num a) => PowS a -> PowS a -> PowS a
fs # gs = case (splitPS fs, splitPS gs) of 
  (f:<>:ft, 0:<>:gt) -> f <:> gt * ft # gs
  (f:<>:ft, g:<>:gt) -> PowS (f:|[]) + gs * ft # gs  -- ft must be polynomial
  (Empty , _)        -> PowS (0:|[])
  (_, Empty)         -> PowS (0:|[])

revert :: (Eq a, Fractional a) => PowS a -> PowS a 
revert fs = case splitPS fs of
  _:<>:PowS (0:|_)   -> error "revert f where f'(0)==0" 
  0:<>:ft            -> rs where rs = 0 <:> PowS (1:|[]) / ft # rs
  f:<>:PowS (f':|[]) -> PowS (-f/f':|[1/f'])
  _                  -> error "revert f where f(0)/=0"

int :: (Fractional a, Enum a) => PowS a -> PowS a 
int fs = PowS (0 :| zipWith (/) (NE.toList $ fromPowS fs) [1..])

diff :: (Num a, Enum a) => PowS a -> PowS a
diff fs = PowS (head u :| tail u) where u = zipWith (*) (NE.tail $ fromPowS fs) [1..]

-- replaces `take` from prelude
takeS :: Int -> PowS a -> PowS a
takeS i s = PowS (NE.fromList $ NE.take i $ fromPowS s)

-- like `takeS` but returns a vanilla list
takeList :: Int -> PowS a -> [a]
takeList i s = NE.take i $ fromPowS s
