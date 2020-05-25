import Control.Arrow

import Data.List
import Data.List.Split
import Data.Maybe

import Scanner
import NumberTheory

-- Input:
--   first line number of cases
--   each case four lines
--     size
--     message
--     cyphertext
--     permutation

main :: IO ()
main = interact $
  lines
  >>> drop 1
  >>> chunksOf 4
  >>> map solve
  >>> unlines

-- solve takes one case
--

solve :: [String] -> String
solve kase = show $ fst $ fromJust $ gcrt klPairs
  where
    message, ctext, perm :: [Integer]
    [_,message,ctext,perm] = map ((map ((+ (-1)) . (read :: String -> Integer))) . words) kase  -- [[Integer]]

    mcPairs, klPairs :: [(Integer,Integer)]
    mcPairs = zip message ctext
    klPairs = map findKl mcPairs
    findKl :: (Integer,Integer) -> (Integer,Integer)
    findKl (m, c) = both toInteger ( fromJust $ elemIndex c cycle, length cycle)
      where
        cycle = cycleFrom perm m

cycleFrom :: [Integer] -> Integer -> [Integer]
cycleFrom perm m = map (toInteger) (
  m' : (takeWhile (/= m') (tail (iterate (perm'!!) m'))))

  where
    m' = fromInteger m
    perm' = map fromInteger perm

both :: (a->b) -> (a,a) -> (b,b)
both f (x, y) = (f x, f y)
