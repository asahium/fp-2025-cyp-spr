module RAList where

import qualified Tree as T
module Tree (fromList, splitAtLevels, null, singleton, deleteMin) where

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
wellFormed = all (either (const True) T.wellFormed) . reverse

-- Flattens a random-access list into a normal list
toList :: RAList a -> [a]
toList = concatMap (either (const []) T.toList) . reverse

-- Generates a random-access list from a normal list
fromList :: [a] -> RAList a
fromList = map (One . T.fromList) . T.splitAtLevels . length

-- Fetches the k-th element of the list
fetch :: Int -> RAList a -> a
fetch k xs = case xs of
  Zero : _ -> error "Index out of bounds"
  One t : _ -> T.fetch k t

-- Checks the list for emptiness
null :: RAList a -> Bool
null = all (either (const True) T.null) . reverse

-- Creates the empty list
nil :: RAList a
nil = []

-- Adds a given element to the head of the list.
-- Should always return a well-formed list.
cons :: a -> RAList a -> RAList a
cons h t = One (T.singleton h) : t

-- Splits the list into a head and a tail, if not empty.
-- The resulting tail should be a well-formed list.
uncons :: RAList a -> Maybe (a, RAList a)
uncons xs = case xs of
  Zero : _ -> Nothing
  One t : ts -> Just (T.fetch 0 t, One (T.deleteMin t) : ts)

-- Updates the k-th element of the list with the given value
update :: Int -> a -> RAList a -> RAList a
update n x xs = case xs of
  Zero : _ -> error "Index out of bounds"
  One t : ts -> One (T.update n x t) : ts
