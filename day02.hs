{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (void)
import Data.Char (digitToInt, isDigit)
import Data.Either (rights)
import Data.Map qualified as M
import Data.Maybe (fromJust)
import Data.Text qualified as T
import Lib (readFile')
import Text.Parsec

type Ball = (String, Int)

type Group = [Ball]

type Balls = [Group]

firstReq :: M.Map String Int
firstReq = M.fromList [("red", 12), ("green", 13), ("blue", 14)]

parser :: (Stream s m Char) => ParsecT s u m (Int, Balls)
parser = do
  game <- string "Game " *> many digit <* char ':'
  balls <- many $
    do
      init <- many (try $ ball <* char ',')
      last <- ball <* (void (char ';') <|> eof)
      return $ last : init

  return (read game, balls)
  where
    ball :: (Stream s m Char) => ParsecT s u m (String, Int)
    ball = do
      count <- char ' ' *> many digit
      color <- char ' ' *> many letter
      return (color, read count :: Int)

first :: (Int, Balls) -> Int
first (game, balls)
  | any (any (\(color, count) -> count > fromJust (M.lookup color firstReq))) balls = 0
  | otherwise = game

second :: (Int, Balls) -> Int
second (_, balls) = M.foldr' (*) 1 $ foldr second' M.empty balls
  where
    second' :: Group -> M.Map String Int -> M.Map String Int
    second' group = M.unionWith max (M.fromList group)

main :: IO ()
main = do
  input <- T.lines <$> readFile' "day02.in"
  putStr "Q1: "
  print . sum . map first . rights . map (parse parser "") $ input
  putStr "Q2: "
  print . sum . map second . rights . map (parse parser "") $ input
