--
-- lab 3.7
--
module DreiSieben where

data GenTree a = GenLeaf a 
	       | GenNode [GenTree a]
               deriving (Eq,Show)

leavesInGenTrees :: GenTree a -> Integer
leavesInGenTrees (GenLeaf _) = 1
leavesInGenTrees (GenNode []) = 0
leavesInGenTrees (GenNode (x:xs)) = leavesInGenTrees x + leavesInGenTrees (GenNode xs)
