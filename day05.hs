import Data.Either (rights)
import Data.List (find)
import Data.Map qualified as M
import Data.Maybe (maybe)
import Data.Text qualified as T
import Lib (readFile', split)
import Text.Parsec

data Mapping = Mapping {dst :: Int, src :: Int, range :: Int}

data Range = Range {start :: Int, end :: Int}

-- i hate this so much

main :: IO ()
main = do
  (seedsRaw : _ : mapsRaw') <- T.lines <$> readFile' "day05.in"
  let mapsRaw = split T.empty mapsRaw'
  let Right seeds = parse parseSeeds "" seedsRaw
  let maps = parseMaps mapsRaw
  putStr "Q1: "
  print . minimum . map (\x -> start . head $ search "seed" maps Range {start = x, end = x}) $ seeds
  putStr "Q2: "
  print . minimum . map (minimum . map start . search "seed" maps) $ seedRanges seeds
  where
    seedRanges :: [Int] -> [Range]
    seedRanges [] = []
    seedRanges (start : range : xs) = Range {start, end = start + range - 1} : seedRanges xs

search :: String -> M.Map String (String, [Mapping]) -> Range -> [Range]
search from m r
  | to == "location" = mapped
  | otherwise = concatMap (search to m) mapped
  where
    Just (to, mappings) = M.lookup from m
    mapped = search' r mappings

-- TODO: make this less scuffed (?)
search' :: Range -> [Mapping] -> [Range]
search' r@Range {start, end} mappings =
  maybe
    [r]
    (mapRange r)
    (find (\Mapping {dst, src, range} -> start < src + range && end >= src) mappings)
  where
    mapRange :: Range -> Mapping -> [Range]
    mapRange r@Range {start, end} Mapping {dst, src, range}
      -- For [] = search range, () = mapping
      --  [___(__]___)
      | start < src && end < src + range = Range {start = dst, end = dst + end - src} : search' Range {start, end = src - 1} mappings
      --  (__[____]__)
      | start >= src && end < src + range = [Range {start = dst + start - src, end = dst + end - src}]
      --  (___[__)___]
      | start >= src && end >= src + range = Range {start = dst + start - src, end = dst + range - 1} : search' Range {start = src + range, end} mappings
      --  [__(____)__]
      | start <= src && end >= src + range = Range {start = dst, end = dst + range - 1} : (search' Range {start, end = src - 1} mappings <> search' Range {start = src + range, end} mappings)

-- Parsing --

parseSeeds :: (Stream s m Char) => ParsecT s u m [Int]
parseSeeds = do
  string "seeds: "
  seed <- many1 digit `sepBy` space
  return $ read <$> seed

parseMaps = foldr parseMaps' M.empty
  where
    parseMaps' :: [T.Text] -> M.Map String (String, [Mapping]) -> M.Map String (String, [Mapping])
    parseMaps' (x : xs) m =
      let Right (from, to) = parse parseHead "" x
          mappings = rights $ map (parse parseMapping "") xs
       in M.insert from (to, mappings) m

parseHead :: (Stream s m Char) => ParsecT s u m (String, String)
parseHead = do
  [from, "to", to] <- many1 letter `sepBy` char '-'
  string " map:"
  return (from, to)

parseMaps :: [[T.Text]] -> M.Map String (String, [Mapping])
parseMapping :: (Stream s m Char) => ParsecT s u m Mapping
parseMapping = do
  [dst, src, range] <- (read <$> many1 digit) `sepBy` space
  return $ Mapping {dst, src, range}
