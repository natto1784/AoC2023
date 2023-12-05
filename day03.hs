import Control.Monad (guard)
import Data.Char (digitToInt, isDigit)
import Data.Text qualified as T
import Lib (readFile')

-- i hate this problem, let's just do an ugly recursion and be done with it
type Symbol = Int

data Number = Number
  { value :: Int,
    start :: Int,
    end :: Int
  }
  deriving (Show)

parseLines :: [T.Text] -> ([[Symbol]], [[Number]])
parseLines lines =
  foldl
    ( \(symbols, numbers) line ->
        let (symbols', numbers') = parseLine line
         in (symbols' : symbols, numbers' : numbers)
    )
    ([], [])
    lines

-- ugly, i know but runs p fast with -O2
parseLine :: T.Text -> ([Symbol], [Number])
parseLine line =
  let (num, start, end, isNum, symbols, numbers) = T.foldl fn (0, 0, 0, False, [], []) line
   in (symbols, if isNum then Number num start (end - 1) : numbers else numbers)
  where
    fn :: (Int, Int, Int, Bool, [Symbol], [Number]) -> Char -> (Int, Int, Int, Bool, [Symbol], [Number])
    fn (num, start, end, isNum, symbols, numbers) char =
      let isNum' = isDigit char
          num' = if isNum' then num * 10 + digitToInt char else 0
          start' = if not isNum' then end + 1 else start
          symbols' =
            if char /= '.' && not isNum'
              then end : symbols
              else symbols
          numbers' =
            if isNum && not isNum'
              then Number num start (end - 1) : numbers
              else numbers
       in (num', start', end + 1, isNum', symbols', numbers')

groups3 :: [a] -> [[a]]
groups3 xs = take 3 xs : groups3 (tail xs)

first :: [[Symbol]] -> [[Number]] -> [[Int]]
first symbols numbers =
  let zipped = zip numbers (init $ groups3 ([] : symbols))
   in do
        (numbersRow, neighbours) <- zipped
        return $ do
          Number value start end <- numbersRow
          guard $ or $ do
            neighboursRow <- neighbours
            return $ or $ do
              symbol <- neighboursRow
              return $ symbol >= start - 1 && symbol <= end + 1
          return value

second :: [[Symbol]] -> [[Number]] -> [Int]
second symbols numbers =
  let zipped = zip symbols (init $ groups3 ([] : numbers))
   in do
        (symbols, neighbours) <- zipped
        symbol <- symbols
        let values = take 2 $ do
              neighboursRow <- neighbours
              Number value start end <- neighboursRow
              guard $ symbol >= start - 1
              guard $ symbol <= end + 1
              return value
        guard $ length values == 2
        return $ product values

main :: IO ()
main = do
  input <- T.lines <$> readFile' "day03.in"
  let (symbols, numbers) = parseLines input
  putStr "Q1: "
  print . sum . concat $ first symbols numbers
  putStr "Q2: "
  print . sum $ second symbols numbers
