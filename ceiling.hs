{-# LANGUAGE DeriveFoldable, DeriveFunctor #-}

import Control.Arrow
import Data.List (sort, group)

data Tree a =
  Empty
  | Tree a (Tree a) (Tree a)
  deriving (Show, Ord, Eq, Foldable, Functor)

insert :: (Ord a) => Tree a -> a -> Tree a
insert Empty        b = Tree b Empty Empty
insert (Tree a r l) b =
  if a < b
  then Tree a (insert r b) l
  else Tree a r (insert l b)

insertL :: Tree Int -> [Int] -> Tree Int
insertL = foldl insert

main :: IO ()
main = interact $
  lines
  >>> drop 1
  >>> map (words
           >>> map read
           >>> foldl insert (Empty :: Tree Int)
           >>> fmap (const ()))
  >>> sort
  >>> group
  >>> length
  >>> show
