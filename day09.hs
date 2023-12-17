import Data.Text qualified as T
import Lib (readFile', tRead)

main :: IO ()
main = do
  input <- map (map tRead . T.words) . T.lines <$> readFile' "day09.in"

  putStr "Q1: "
  print . sum $ map (sum . map last . extrap) input

  putStr "Q2: "
  print . sum $ map (sum . map last . extrap . reverse) input

extrap :: [Int] -> [[Int]]
extrap xs = take (length xs) $ iterate first' xs
  where
    first' :: [Int] -> [Int]
    first' xs = zipWith (-) (tail xs) xs
