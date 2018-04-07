module Main where

import PowerSerious

exps :: PowS
exps = 1 + int exps

sins :: PowS
sins = int coss

coss :: PowS
coss = 1 - int sins

-- tans :: PowS
-- tans = revert $ int (1 / PowS' [1,0,1])

tans :: PowS
tans = sins / coss

pascal :: PowS2
pascal = 1 / PowS' [1, - PowS' [1,1]]

main :: IO ()
main = print $ takeS 10 pascal
