{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module GraphSearch where
import Data.Map as Map hiding (filter, null, drop)
import Data.Set as Set hiding (filter, null, drop)

data Node = Node
        {   name     :: String,
            children :: [Node],
            pos      :: (Float, Float)
        }
        deriving Ord

instance Eq Node where
    n1 == n2 = name n1 == name n2

instance Show Node where
    show n = show $ name n

data Graph = Graph (Map Node (Set Node)) | Empty

nodes :: Graph -> [Node]
nodes (Graph g) = keys g

{-
    Checks if a node is in a neighbor list
-}
inList :: Node -> [Node] -> Bool
inList _ [] = False
inList a (b:bs) = a == b || inList a bs       -- skriv om med fold for effektivitet?


{-
    Breadth-First-Search

    graph
    start node
    list of visited nodes
    list of queued nodes
    returns list of nodes in order of exploration

-}
bfs :: Graph -> Node -> [Node] -> [Node] -> [Node]
bfs Empty _ _ _ = []
bfs g start visited queue =
    if visited == nodes g || (null queue && all (`inList` (queue ++ visited)) (children start))--(null queue && null (children start))
    then visited ++ [start]
    else
    bfs g next visited' queue' where
        visited' = visited ++ [start]
        queue'' = queue ++ filter (not . (`inList` (queue ++ visited'))) (children start)
        next = head queue''
        queue' = tail queue''


dfs :: Graph -> Node -> [Node] -> [Node] -> [Node]
dfs Empty _ _ _ = []
dfs g start visited queue =
    if visited == nodes g ||  (null queue && all (`inList` (queue ++ visited)) (children start))
    then visited ++ [start]
    else
    dfs g next visited' queue' where
        visited' = visited ++ [start]
        queue'' = filter (not . (`inList` (queue ++ visited'))) (children start) ++ queue
        next = head queue''
        queue' = tail queue''




a = Node "A" [b, c] (-200, -100)
b = Node "B" [d,b] (-100, 150)
c = Node "C" [e] (0, -200)
d = Node "D" [c, e] (50, 100)
e = Node "E" [] (150, 0)


graph = Graph (Map.fromList [
    (a, Set.fromList $ children a),
    (b, Set.fromList $ children b),
    (c, Set.fromList $ children c),
    (d, Set.fromList $ children d),
    (e, Set.fromList $ children e)])
