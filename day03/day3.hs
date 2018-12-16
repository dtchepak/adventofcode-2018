{-# LANGUAGE TupleSections #-}
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.List as List

type ClaimMap = Map (Int, Int) Int

data Claim = Claim {
    _number :: !Int
    , _x :: !Int
    , _y :: !Int
    , _width :: !Int
    , _height :: !Int
} deriving (Show, Eq)

breakOn :: Eq a => a -> [a] -> ([a], [a])
breakOn b xs =
    (takeWhile (/= b) xs, drop 1 (dropWhile (/= b) xs))

-- Claim format: 
--     "#1233 @ 749,804: 24x14"

-- | Partial code to produce a claim from a string. Yes I feel bad.
unsafeParseClaim :: String -> Claim
unsafeParseClaim s =
    let [num, _, coords, size] = words s
        (ptX, ptY') = breakOn ',' coords
        ptY = takeWhile (/= ':') ptY'
        (w, h) = breakOn 'x' size
    in Claim
        (read (drop 1 num))  -- drop '#' from a value like "#1234"
        (read ptX)
        (read ptY)
        (read w)
        (read h)

toClaimMap :: Claim -> ClaimMap
toClaimMap (Claim _ x y w h) =
    let pts = [ x .. x+w-1 ] >>= \x' -> (x',) <$> [ y .. y+h-1 ]
    in Map.fromList ( (,1) <$> pts )

claims :: IO [Claim]
claims = fmap unsafeParseClaim . lines <$> readFile "input.txt"

example :: [Claim]
example = unsafeParseClaim <$>
    ["#1 @ 1,3: 4x4"
    ,"#2 @ 3,1: 4x4"
    ,"#3 @ 5,5: 2x2"
    ]

hasNoOverlaps :: ClaimMap -> Claim -> Bool
hasNoOverlaps cm =
    null . Map.toList . Map.filter (>1) . Map.intersection cm . toClaimMap

main :: IO ()
main = do
    c <- claims
    let claimMap = Map.unionsWith (+) (toClaimMap <$> c)
    let overlapClaims = Map.filter (> 1) claimMap
    print c
    print "=============="
    print "Overlaps:"
    print "=============="
    print (Map.size overlapClaims)
    print (List.find (hasNoOverlaps claimMap) c)



