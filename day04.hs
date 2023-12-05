import Data.Either (rights)
import Data.List (intersect)
import Data.Text qualified as T
import Lib (readFile')
import Text.Parsec

parser :: (Stream s m Char) => ParsecT s u m ([Int], [Int])
parser =
  do
    card <- string "Card" *> many1 space *> many1 digit <* char ':'
    many1 space
    winning <- many digit `sepBy` many1 space
    char '|'
    many1 space
    mine <- many digit `sepBy` many1 space

    return (read <$> init winning, read <$> mine)

main :: IO ()
main = do
  input <- T.lines <$> readFile' "day04.in"
  let parsed = rights . map (parse parser "") $ input

  putStr "Q1: "
  print
    . sum
    . map
      ( (\x -> if x < 0 then 0 else 2 ^ x)
          . flip (-) 1
          . length
          . uncurry intersect
      )
    $ parsed

  putStr "Q2: "
  print
    . sum
    . consequence
    . map
      ( \(x, y) ->
          length $
            intersect x y
      )
    $ parsed
  where
    -- TODO: dont bruteforce (its slow)
    consequence :: [Int] -> [Int]
    consequence [] = []
    consequence (x : xs) = 1 + sum (take x $ consequence xs) : consequence xs
