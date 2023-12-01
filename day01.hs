{-# LANGUAGE OverloadedStrings #-}

import Data.Char (digitToInt, isDigit)
import Data.Text qualified as T
import Lib (readFile')

first :: T.Text -> Int
first s = f * 10 + l
  where
    values = T.filter isDigit s
    f = (digitToInt . T.head) values
    l = (digitToInt . T.last) values

second' :: T.Text -> [Int]
second' s
  | T.null s = []
  | T.isPrefixOf "one" s = 1 : second' (T.tail s)
  | T.isPrefixOf "two" s = 2 : second' (T.tail s)
  | T.isPrefixOf "three" s = 3 : second' (T.tail s)
  | T.isPrefixOf "four" s = 4 : second' (T.tail s)
  | T.isPrefixOf "five" s = 5 : second' (T.tail s)
  | T.isPrefixOf "six" s = 6 : second' (T.tail s)
  | T.isPrefixOf "seven" s = 7 : second' (T.tail s)
  | T.isPrefixOf "eight" s = 8 : second' (T.tail s)
  | T.isPrefixOf "nine" s = 9 : second' (T.tail s)
  | isDigit (T.head s) = digitToInt (T.head s) : second' (T.tail s)
  | otherwise = second' (T.tail s)

second :: T.Text -> Int
second s = head values * 10 + last values
  where
    values = second' s

main :: IO ()
main = do
  input <- T.lines <$> readFile' "day01.in"
  putStr "Q1: "
  print . sum . map first $ input
  putStr "Q2: "
  print . sum . map second $ input
