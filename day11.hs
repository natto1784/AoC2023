import Data.List (tails)

type Coord = (Int, Int)

-- slow but dont care
main :: IO ()
main = do
  input <- lines <$> readFile "day11.in"

  let galaxies = [(x, y) | (row, x) <- zip input [0 ..], ('#', y) <- zip row [0 ..]]

  let emptyRows = [r | r <- [0 .. length input], r `notElem` map fst galaxies]
  let emptyCols = [c | c <- [0 .. length $ head input], c `notElem` map snd galaxies]

  let galaxyPairs = [(x, y) | (x : xs) <- tails galaxies, y <- xs]

  putStr "Q1: "
  print . sum $ map (uncurry $ distance emptyRows emptyCols 2) galaxyPairs

  putStr "Q2: "
  print . sum $ map (uncurry $ distance emptyRows emptyCols 1000000) galaxyPairs

distance :: [Int] -> [Int] -> Int -> Coord -> Coord -> Int
distance rs cs f (x1, y1) (x2, y2) =
  abs (x1 - x2)
    + abs (y1 - y2)
    + (f - 1) * length (filter (between x1 x2) rs)
    + (f - 1) * length (filter (between y1 y2) cs)

between :: Int -> Int -> Int -> Bool
between a b x = x > min a b && x < max a b
