module Binary where

-- Checks that the given list is a valid reversed binary representation
isValid :: [Int] -> Maybe [Int]
isValid xs = if all (`elem` [0, 1]) xs then Just xs else Nothing

-- Generates an infinite list of reversed binary representations of natural numbers in order
nums :: [[Int]]
nums = map reverse $ iterate (\xs -> if last xs == 0 then xs ++ [1] else xs ++ [0]) [0]

-- Computes the reversed binary representation of a positive number: no leading zeros
-- toBinary 6 == Just [0,1,1]
-- toBinary 0 == Just [0]
-- toBinary (-10) == Nothing
toBinary :: Int -> Maybe [Int]
toBinary n
  | n < 0     = Nothing
  | otherwise = Just $ reverse $ unfoldr (\x -> if x == 0 then Nothing else Just (x `mod` 2, x `div` 2)) n


-- Computes the integer from a given reversed binary representation
-- fromBinary [0,1,1] == Just 6
-- fromBinary [0] == Just 0
-- fromBinary [1,2,3] == Nothing
fromBinary :: [Int] -> Maybe Int
fromBinary xs = if all (`elem` [0, 1]) xs then Just $ sum $ zipWith (*) xs (iterate (*2) 1) else Nothing
