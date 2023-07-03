module Main where
import Data.Char (ord, isLower)
import Data.List (nub)


main :: IO ()
main = do
  file <- readFile "d3.txt"
  print $ solve1 file
  print $ solve2 file

type Rucksack = (String, String)

compartments :: String -> Rucksack
compartments str = let n = length str `div` 2
                   in splitAt n str

findDup :: Rucksack -> [Char]
findDup (f, s) = nub $ filter (`elem` s) f

priority :: Char -> Int
priority c = if isLower c
  then ord c - ord 'a' + 1
  else ord c - ord 'A' + 27

solve1 :: String -> Int
solve1 = sum 
       . map priority 
       . concatMap (findDup . compartments)
       . lines


groupN :: Int -> [a] -> [[a]]
groupN n [] = []
groupN 0 xs = []
groupN n xs = take n xs : groupN n (drop n xs)

findBadge :: Eq a => [[a]] -> a
findBadge [a,b,c] = head $ filter common a
  where common x = x`elem`b && x`elem`c

solve2 :: String -> Int
solve2 = sum
       . map (priority . findBadge) 
       . groupN 3
       . lines
