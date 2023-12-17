import Data.Either (rights)
import Data.Map qualified as M
import Data.Text qualified as T
import Lib (readFile')
import Text.Parsec

main :: IO ()
main = do
  (directionsRaw : _ : mapsRaw) <- T.lines <$> readFile' "day08.in"
  let directions = cycle $ T.unpack directionsRaw
  let maps' = rights $ map (parse parseMap "") mapsRaw
  let maps = M.fromList maps'

  putStr "Q1: "
  print $ zzz directions maps 0 "AAA"

  putStr "Q2: "
  print . foldl1 lcm . map (xxz directions maps 0) . filter (\[_, _, c] -> c == 'A') . map fst $ maps'

-- dumb and good recursion
zzz :: String -> M.Map String (String, String) -> Int -> String -> Int
zzz (d : ds) m acc str
  | next == "ZZZ" = acc + 1
  | otherwise = zzz ds m (acc + 1) next
  where
    Just (left, right) = M.lookup str m
    next = if d == 'L' then left else right

xxz :: String -> M.Map String (String, String) -> Int -> String -> Int
xxz (d : ds) m acc str
  | [_, _, 'Z'] <- next = acc + 1
  | otherwise = xxz ds m (acc + 1) next
  where
    Just (left, right) = M.lookup str m
    next = if d == 'L' then left else right

-- Parsing --

parseMap :: (Stream s m Char) => ParsecT s u m (String, (String, String))
parseMap = do
  a <- many1 letter <* string " = ("
  b <- many1 letter <* string ", "
  c <- many1 letter <* char ')'
  return (a, (b, c))
