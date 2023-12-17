data Race = Race {time :: Int, distance :: Int} deriving (Show)

main :: IO ()
main = do
  [_ : time', _ : distance'] <- map words . lines <$> readFile "day06.in"
  
  let races = zipWith (\t d -> Race {time = read t, distance = read d}) time' distance'
  let bigRace = Race {time = read $ concat time', distance = read $ concat distance'}
  
  putStr "Q1: "
  print . product . map race $ races
  putStr "Q2: "
  print $ race bigRace

-- we need (T - a) * a > D or a^2 - T*a + D < 0 i.e, a quadratic inequality
race :: Race -> Int
race Race {time, distance} =
  let (x, y) = roots
   in ceiling (x - 1) - floor (y + 1) + 1
  where
    -- roots
    roots :: (Double, Double)
    roots =
      let disc = fromIntegral $ time * time - 4 * distance
       in ((fromIntegral time + sqrt disc) / 2, (fromIntegral time - sqrt disc) / 2)
