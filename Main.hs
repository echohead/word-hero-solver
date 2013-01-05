import Data.Map (Map)
import Data.Sequence
import qualified Data.Map as Map
import System.Environment (getArgs)

data CharMap = CharMap (Map Char CharMap)
             | EndofWord
             deriving (Show)

graph :: [String] -> CharMap
graph wds =
  foldl addWord (CharMap Map.empty) wds
  where
    addWord :: CharMap -> String -> CharMap
    addWord (CharMap m) "" = CharMap $ Map.insert '$' EndofWord m
    addWord (CharMap m) (x:xs) = case Map.lookup x m of
                         Just m' -> addWord m' xs
                         Nothing -> CharMap $ Map.insert x (addWord (CharMap Map.empty) xs) m

lookup :: Char -> CharMap -> Maybe CharMap
lookup c (CharMap m) = Map.lookup c m

findWords :: CharMap -> String -> String
findWords g i = show $ map (fromStart (fromList i) g  "") [0 .. 15]

fromStart :: Seq Char -> CharMap -> String -> Int -> [String]
fromStart board g sofar i =
  case Main.lookup c g of
    Just g' -> concat ((wordIfEnd g' (c:sofar)):(map (fromStart (update i '_' board) g' (c:sofar)) (nbrs i)))
    Nothing -> []
  where
    c = index board i
    wordIfEnd g sofar =
      case Main.lookup '$' g of
        Just _  -> [sofar]
        Nothing -> []

main = do
  args <- getArgs
  case args of
    [dictPath] -> do
        cnts <- readFile dictPath
        let wds = lines cnts
        let g = graph wds
        putStrLn $ show wds
        putStrLn $ show $ g
        putStrLn $ "enter the letters:"
        input <- getLine
        putStrLn $ findWords g input
    _          -> putStrLn "error: exactly one argument expected"


nbrs :: Int -> [Int]
nbrs 0 = [1, 4, 5]
nbrs 1 = [0, 4, 5, 6, 2]
nbrs 2 = [1, 5, 6, 7, 3]
nbrs 3 = [2, 6, 7]
nbrs 4 = [0, 1, 5, 9, 8]
nbrs 5 = [0, 1, 2, 6, 10, 9, 8, 4]
nbrs 6 = [1, 2, 3, 7, 11, 10, 9, 5]
nbrs 7 = [11, 10, 6, 2, 3]
nbrs 8 = [4, 5, 9, 13, 12]
nbrs 9 = [4, 5, 6, 10, 14, 13, 12, 8]
nbrs 10 = [5, 6, 7, 11, 15, 14, 13, 9]
nbrs 11 = [15, 14, 10, 6, 7]
nbrs 12 = [8, 9, 13]
nbrs 13 = [12, 8, 9, 10, 14]
nbrs 14 = [13, 9, 10, 11, 15]
nbrs 15 = [14, 10, 11]

