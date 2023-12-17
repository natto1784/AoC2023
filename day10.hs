import Data.List (find)
import Data.Map qualified as M
import Data.Maybe (catMaybes, isNothing, mapMaybe)
import Data.Set qualified as S
import Lib (buildGrid, readFile')

data Direction = East | West | North | South deriving (Show, Eq)

type Coord = (Int, Int)

main :: IO ()
main = do
  grid <- buildGrid . lines <$> readFile "day10.in"
  let [((sx, sy), _)] = M.toList $ M.filter (== 'S') grid
  let path =
        head $
          mapMaybe
            (uncurry $ travel grid [])
            [ ((sx - 1, sy), North),
              ((sx, sy + 1), East)
            ]

  putStr "Q1: "
  print $ div (length path) 2

  putStr "Q2: "
  print $ pick (shoelace path) (length path)

travel :: M.Map Coord Char -> [Coord] -> Coord -> Direction -> Maybe [Coord]
travel grid a c@(x, y) d
  | isNothing (M.lookup c grid) || cur == '.' = Nothing
  | cur == 'S' = Just $ c : a
  | (cur, d) `elem` [('L', West), ('|', North), ('J', East)] = travel grid (c : a) (x - 1, y) North
  | (cur, d) `elem` [('F', West), ('|', South), ('7', East)] = travel grid (c : a) (x + 1, y) South
  | (cur, d) `elem` [('L', South), ('-', East), ('F', North)] = travel grid (c : a) (x, y + 1) East
  | (cur, d) `elem` [('J', South), ('-', West), ('7', North)] = travel grid (c : a) (x, y - 1) West
  where
    cur = grid M.! c

-- https://en.wikipedia.org/wiki/Shoelace_formula
-- this just uses trapezoids/triangles from consecutive points
shoelace :: [Coord] -> Int
shoelace xs = flip div 2 . abs . sum $ zipWith (\(x1, y1) (x2, y2) -> (x1 * y2) - (x2 * y1)) xs (tail $ cycle xs)

-- https://en.wikipedia.org/wiki/Pick%27s_theorem
-- area -> path length -> integers inside
pick :: Int -> Int -> Int
pick a b = (a + 1) - div b 2
