module Tree where

data BinTree a = Leaf a | Branch (BinTree a) (BinTree a)
    deriving (Show, Eq)


{-
    Calculates the depth of a binary tree recursively
-}
depth :: BinTree a -> Int -> Int
depth (Leaf a) i = i
depth (Branch l r) i = max (depth l i+1) (depth r i+1)

