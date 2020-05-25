{-# LANGUAGE OverloadedStrings #-}

import Control.Arrow

import Data.List
import Data.Maybe
import Control.Applicative
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as C

import qualified ScannerBS as S


data Subway = Subway [Subway]
  deriving (Show,Ord,Eq)

cannonical :: Subway -> Subway
cannonical (Subway xs)   = Subway $ sort (map cannonical xs)

parseSubway :: Parser Subway
parseSubway = Subway <$> (string "0" *> many parseSubway <* string "1")

str2sub :: C.ByteString -> Subway
str2sub s = fromJust $ maybeResult $ parse parseSubway (C.cons '0' (C.snoc s '1'))

main :: IO ()
main = C.interact $
  S.runScanner (S.numberOf (S.two S.str))
  >>> map solve >>> C.unlines

solve :: [C.ByteString] -> C.ByteString
solve = (["","same","different"] !!) . length . group . map (cannonical . str2sub)
