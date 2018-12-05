import qualified Data.Map.Strict as M
import Data.List (foldl')
import Data.Maybe (catMaybes)

charCounts :: String -> M.Map Char Int
charCounts =
    foldl' (\acc x -> M.insertWith (+) x 1 acc) M.empty

checksum :: [String] -> Int
checksum =
    uncurry (*) . foldl' (\(twos, threes) s ->
        let counts = charCounts s
            has x = if (null (M.filter (==x) counts)) then 0 else 1
        in (twos + has 2, threes + has 3)) (0,0)

minusDiff :: String -> String -> String
minusDiff first second =
    catMaybes (zipWith (\a b -> if (a/=b) then Nothing else Just a) first second)

diff :: String -> String -> [Char]
diff first second =
    catMaybes (zipWith (\a b -> if (a/=b) then Just a else Nothing) first second)

diffs :: [String] -> [(String, [String])]
diffs xs =
    xs >>= \x -> [(x, diff x <$> xs)]

findOnes :: [String] -> [(String, [String])]
findOnes xs =
    xs >>= \x ->
        let ones = filter ((==1) . length) (diff x <$> xs)
        in if (not . null $ ones) then
            [(x, ones)]
        else []


main :: IO ()
main = do
    input <- lines <$> readFile "input.txt"
    print $ checksum input
    let differByOne = findOnes input
    print differByOne
    let differByOne' = map fst differByOne
    print (zipWith minusDiff differByOne' (drop 1 differByOne'))


