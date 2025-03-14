module RAList where

import qualified Tree as T

-- This is an implementation of a random-access list.
-- It's possible to achieve logarithmic access by representing a list as a sequence of perfect trees.
-- The sequence should correspond to the reversed binary representation of the size of the list.
-- For example, the list ['a'..'f'] of size 6 (011) corresponds to the following sequence:
-- [ Zero
-- , One (Node 2 (Leaf 'a') (Leaf 'b'))
-- , One (Node 4 (Node 2 (Leaf 'c') (Leaf 'd')) (Node 2 (Leaf 'e') (Leaf 'f')))
-- ]
-- Any operations over the random-access list should preserve its well-formedness.

-- Encodes "bits" in the sequence
data Digit a
  = Zero
  | One (T.Tree a)
  deriving (Show, Eq)

-- Encodes a list as the sequence of perfect trees
type RAList a = [Digit a]

-- Checks that the random-access list has correct structure
wellFormed :: RAList a -> Bool
wellFormed = all (\x -> case x of One t -> T.wellFormed t; Zero -> True)

-- Flattens a random-access list into a normal list
toList :: RAList a -> [a]
toList = concatMap (\x -> case x of One t -> T.toList t; Zero -> [])

-- Generates a random-access list from a normal list
fromList :: [a] -> RAList a
fromList [] = []
fromList xs = foldr cons nil xs

-- Fetches the k-th element of the list
fetch :: Int -> RAList a -> a
fetch _ [] = error "Index out of bounds"
fetch k (Zero : xs) = fetch k xs
fetch k (One t : xs)
  | k < T.size t = T.fetch k t
  | otherwise    = fetch (k - T.size t) xs

-- Checks the list for emptiness
null :: RAList a -> Bool
null = all (== Zero)

-- Creates the empty list
nil :: RAList a
nil = []

-- Adds a given element to the head of the list.
-- Should always return a well-formed list.
cons :: a -> RAList a -> RAList a
cons x (One t1 : One t2 : ts) = Zero : cons x (One (T.node t1 t2) : ts)
cons x ts = One (T.Leaf x) : ts

-- Splits the list into a head and a tail, if not empty.
-- The resulting tail should be a well-formed list.
uncons :: RAList a -> Maybe (a, RAList a)
uncons [] = Nothing
uncons (Zero : ts) = uncons ts
uncons (One t : ts) =
  let xs = T.toList t
  in case xs of
      [] -> Nothing
      (y:ys) -> Just (y, if null ys then Zero : ts else One (buildTree ys) : ts))

-- Updates the k-th element of the list with the given value
update :: Int -> a -> RAList a -> RAList a
update _ _ [] = error "Index out of bounds"
update k x (Zero : xs) = Zero : update k x xs
update k x (One t : xs)
  | k < T.size t = One (T.update k x t) : xs
  | otherwise    = One t : update (k - T.size t) x xs
