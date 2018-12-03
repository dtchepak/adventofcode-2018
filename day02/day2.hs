import qualified Data.Map.Strict as M
import Data.List (foldl')

charCounts :: String -> M.Map Char Int
charCounts =
    foldl' (\acc x -> M.insertWith (+) x 1 acc) M.empty

checksum :: [String] -> Int
checksum =
    uncurry (*) . foldl' (\(twos, threes) s ->
        let counts = charCounts s
            has x = if (null (M.filter (==x) counts)) then 0 else 1
        in (twos + has 2, threes + has 3)) (0,0)

main :: IO ()
main = do
    input <- lines <$> readFile "input.txt"
    print $ checksum input
