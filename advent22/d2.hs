module Main where
import Data.Char (ord)

main :: IO ()
main = do
  file <- readFile "d2.txt"
  print $ solve1 file
  print $ solve2 file

data RPS = R | P | S
  deriving (Show, Eq, Enum)

data Outcome = L | D | W
  deriving (Show, Eq, Enum)

pointOut :: Outcome -> Int
pointOut = (*3) . fromEnum

pointRPS :: RPS -> Int
pointRPS = (+1) . fromEnum



type Round1 = (RPS, RPS)

decode1 :: String -> Round1
decode1 [a, ' ', x] = (decodeABC a, decodeXYZ x) where
  decodeABC x = toEnum $ ord x - ord 'A'
  decodeXYZ x = toEnum $ ord x - ord 'X'

outcome :: Round1 -> Outcome
outcome (S,R)          = W
outcome (P,S)          = W
outcome (R,P)          = W
outcome (x,y) | x == y = D
outcome _              = L

points1 :: Round1 -> Int
points1 r = pointOut (outcome r) + pointRPS (snd r)

solve1 :: String -> Int
solve1 = sum . map (points1 . decode1) . lines


type Round2 = (RPS, Outcome)

decode2 :: String -> Round2
decode2 [a, ' ', x] = (decodeABC a, decodeXYZ x) where
  decodeABC x = toEnum $ ord x - ord 'A'
  decodeXYZ x = toEnum $ ord x - ord 'X'

figure :: Round2 -> RPS
figure (a,D) = a
figure (a,W) = toEnum $ mod (fromEnum a + 1) 3
figure (a,L) = toEnum $ mod (fromEnum a - 1) 3

points2 :: Round2 -> Int
points2 r = pointOut (snd r) + pointRPS (figure r)

solve2 :: String -> Int
solve2 = sum . map (points2 . decode2) . lines