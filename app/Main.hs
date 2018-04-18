module Main where

import PowerSerious
import Control.Comonad
import Data.List

exps :: PowS
exps = 1 + int exps

sins :: PowS
sins = int coss

coss :: PowS
coss = 1 - int sins

tans :: PowS
tans = revert $ int (1 / PowS' [1,0,1])

pascal :: PowS2
pascal = 1 / PowS' [1, PowS' [1,1]]

supernecklace' :: Int -> Int -> Int
supernecklace' degree size = length . last . take degree $ iterate
                             (\s -> enumFromTo 1 . length . tail $ subsequences s)
                             (enumFromTo 1 size)

-- supernecklace :: Int -> PowS' Int
-- supernecklace n = iterate (liftW2 $ tail . subsequences) (PowS' [1..n])

main :: IO ()
main = print $ takeS 10 pascal
