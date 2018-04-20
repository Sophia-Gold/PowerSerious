module Main where

import PowerSerious
import Data.List

exps :: PowS Rational
exps = 1 + int exps

sins :: PowS Rational
sins = int coss

coss :: PowS Rational
coss = 1 - int sins

tans :: PowS Rational
tans = revert $ int (1 / PowS [1,0,1])

pascal :: PowS (PowS Rational)
pascal = 1 / PowS [1, - PowS [1,1]]

supernecklace' :: Int -> Int -> Int
supernecklace' degree size = length . last . take degree $ iterate
                             (\s -> enumFromTo 1 . length . tail $ subsequences s)
                             (enumFromTo 1 size)

-- degree 3
-- supernecklace :: Int -> PowS (PowS (PowS (Int)))
supernecklace :: Int -> PowS [[Int]]
supernecklace n = (tail . subsequences) <$> (concat . tail . subsequences) <$> PowS [[1..n]]

main :: IO ()
main = print $ takeS 10 pascal
