module Main where
import Data.List

main :: IO ()
main = do
  file <- readFile "d1.txt"
  let solved = solve file
  print $ part1 solved
  print $ part2 solved

part1 :: [Int] -> Int
part1 = maximum

part2 :: [Int] -> Int
part2 = sum . take 3 . reverse . sort

solve :: String -> [Int]
solve = map (sum . map read) -- convert cals to ints and sum them
      . join (not . null)    -- join the lines refeing to the same elf
      . lines                -- split into lines

-- join a list into a list of lists that satisfie a predicate p
join :: (a -> Bool) -> [a] -> [[a]]
join _ [] = []
join p xs = let (t,d) = span p xs
            in t : join p (drop 1 d)