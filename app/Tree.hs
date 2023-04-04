module Tree where

data BinTree a = Leaf a | Branch (BinTree a) (BinTree a)
    deriving (Show, Eq)


{-
    Calculates the depth of a binary tree recursively
-}
depth :: BinTree a -> Int
depth (Leaf a) = 1
depth (Branch l r) = max (depth l +1) (depth r +1)

