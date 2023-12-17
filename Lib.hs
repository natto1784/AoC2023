module Lib (readFile', split, count, tRead) where

import Data.ByteString qualified as B (readFile)
import Data.Text (Text)
import Data.Text.Encoding qualified as T (decodeUtf8)
import Data.Text.Read qualified as TR

-- IO --
readFile' :: FilePath -> IO Text
readFile' f = T.decodeUtf8 <$> B.readFile f

-- Data.Text --
tRead :: Text -> Int
tRead = (\(Right (n, _)) -> n) . TR.signed TR.decimal

-- Utility --
split :: (Eq a) => a -> [a] -> [[a]]
split delimiter list =
  let (before, after) = break (== delimiter) list
   in before : case after of
        [] -> []
        (_ : xs) -> split delimiter xs

count :: (Eq a) => [a] -> a -> Int
count xs x = length $ filter (x ==) xs
