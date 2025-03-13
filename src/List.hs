module List where
import Data.List (delete)

-- Generates all permutations of the given list.
perms :: Eq a => [a] -> [[a]]
perms [] = [[]]
perms xs = [x:ys | x <- xs, ys <- perms (delete x xs)]

-- Concatenate the shortest prefix of xss whose total sum is positive.
-- If no sum is positive, then the whole list is concatenated.
collapse :: [[Int]] -> [Int]
collapse xss = foldl (\acc xs -> if sum xs > 0 then xs else acc) [] xss
