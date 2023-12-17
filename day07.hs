import Data.Char (ord)
import Data.List (group, nub, sort, sortBy)
import Data.Ord (comparing)
import Data.Text qualified as T
import Lib (count)

-- yes, this is slow
-- i dont have time for a cuter (and faster) answer :<
-- Update: I found a solution on reddit that uses some cool default GHC extensions and is much faster, i am not going to use it, but feel free to look it up
main :: IO ()
main = do
  input' <- map words . lines <$> readFile "day07.in"
  let bids = map (\[x, y] -> (x, read y)) input'

  putStr "Q1: "
  print
    . sum
    . zipWith (*) [1 ..]
    . map snd
    . sortBy (comparing (handKind . fst) <> comparing (map strength . fst))
    $ bids

  putStr "Q2: "

  print
    . sum
    . zipWith (*) [1 ..]
    . map snd
    . sortBy
      ( comparing
          (\(hand, _) -> maximum [handKind (map repl hand) | x <- nub hand, let repl c = if c == 'J' then x else c])
          <> comparing (map strength2 . fst)
      )
    $ bids

strength :: Char -> Int
strength c = case c of
  'A' -> 13
  'K' -> 12
  'Q' -> 11
  'J' -> 10
  'T' -> 9
  _ -> ord c - ord '1'

strength2 :: Char -> Int
strength2 c = case c of
  'A' -> 13
  'K' -> 12
  'Q' -> 11
  'J' -> 0
  'T' -> 9
  _ -> ord c - ord '1'

handKind :: String -> Int
handKind hand = case sort $ map length $ group $ sort hand of
  [5] -> 6
  [1, 4] -> 5
  [2, 3] -> 4
  [1, 1, 3] -> 3
  [1, 2, 2] -> 2
  [1, 1, 1, 2] -> 1
  _ -> 0
