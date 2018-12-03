import qualified Text.Read as R
import Data.Maybe (catMaybes)
import Data.List (foldl', scanl')

-- Partial. I'm a bad person and I feel bad.
adjustmentToInt :: String -> Int
adjustmentToInt s =
    if take 1 s == "+" then
        read (drop 1 s)
    else read s

firstDupe :: [Int] -> Maybe Int
firstDupe =
    let step _ [] = Nothing
        step seen (x:xs) =
            if x `elem` seen then Just x
            else step (x:seen) xs
    in step []

main :: IO ()
main = do
    input <- fmap adjustmentToInt . lines <$> readFile "input.txt"
    print $ sum input
    print $ firstDupe (scanl' (+) 0 (cycle input))
