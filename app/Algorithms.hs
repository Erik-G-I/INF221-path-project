{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}


module Algorithms where
import Control.Monad.Trans.Reader
import Data.Map as Map hiding (filter)
import Data.Set as Set hiding (filter)

{-
    Each node has:
        a name
        a distance from the start
        a list of neighbors
        a previous node that has the shortest path to the current node
-}
data Node = Node
    {   name      ::   Char,
        dist      ::   Int,
        neighbors ::   [Node],
        parent    ::   Node
    }

instance Ord Node where
    n1 < n2 = dist n1 < dist n2
    n1 > n2 = dist n1 > dist n2
    n1 <= n2 = dist n1 <= dist n2

instance Eq Node where
    n1 == n2 = name n1 == name n2

instance Show Node where
    show (Node name _ _ _) = show name



a = Node 'A' 0 [b, c, f] a
b = Node 'B' 1000 [e] b
c = Node 'C' 1000 [d] c
d = Node 'D' 1000 [] d
e = Node 'E' 1000 [d] e
f = Node 'F' 1000 [d] f

g = Graph (Map.fromList [(a, Set.fromList [b, c]), (b, Set.empty), (c, Set.singleton d), (d, Set.empty)])

-- A graph of nodes can be represented as map of nodes and their shortest path
data Graph = Graph (Map Node (Set Node)) | Empty


{-
    Checks if a node is in a neighbor list
-}
inList :: Node -> [Node] -> Bool
inList _ [] = False
inList a (b:bs) = name a == name b || inList a bs       -- skriv om med fold for effektivitet?

{-
    Takes a parent node and child node, and updates the distance and parent of the child node.
    If the new distance is not shorter than the current distance of the child node, nothing happens
-}
updateDist :: Node -> Node -> Node
updateDist n1 n2 =
        if dist n1 +1 < dist n2
        then Node (name n2) (dist n1 +1) (neighbors n2) n1
        else n2

{-
    Traces a shortest path to a node in an explored graph
-}
tracePath :: Node -> [Node] -> [Node]
tracePath n xs =
    if m == n
    then xs'
    else tracePath m xs' where
        m = parent n
        xs' = n : xs

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
    where
        smaller = [a | a <- xs, a <=x]
        larger = [b | b <- xs, b >x]

{-
    Breadth-First-Search always explores the last explored node.
    Graph
    Current Node
    End Node
    Visited
    Queue
    Current Path
    Returns Shortest Path
-}
bfs :: Graph -> Node -> Node -> [Node] -> [Node] -> [Node]
bfs Empty _ _ _ _ = []

bfs g currentNode endNode visited queue =
    if currentNode == endNode
    then tracePath currentNode []
    else bfs g nextNode endNode visited' queue'' where
        nbors = Prelude.map (updateDist currentNode) (neighbors currentNode)
        queue' = queue ++ filter (not . (`inList` queue)) nbors
        nextNode = head queue'
        queue'' = tail queue'
        visited' = visited ++ [currentNode]



{-
    Depth-First-Search always explores the most recent explored node.
    Graph
    Current Node
    End Node
    Visited
    Queue
    Current Path
    Returns Shortest Path
-}
dfs :: Graph -> Node -> Node -> [Node] -> [Node] -> [Node]
dfs Empty _ _ _ _ = []

dfs g currentNode endNode visited queue =
    if currentNode == endNode
    then tracePath currentNode []
    else dfs g nextNode endNode visited' queue'' where
        nbors = Prelude.map (updateDist currentNode) (neighbors currentNode)
        queue' = filter (not . (`inList` queue)) nbors ++ queue
        nextNode = head queue'
        queue'' = tail queue'
        visited' = visited ++ [currentNode]

{-
    Dijkstra's algorithm 
-}
dijkstra :: Graph -> Node -> Node -> [Node] -> [Node] -> [Node]
dijkstra Empty _ _ _ _ = []

dijkstra g currentNode endNode visited queue =
    if currentNode == endNode
    then tracePath currentNode []
    else dijkstra g nextNode endNode visited' queue'' where
        nbors = Prelude.map (updateDist currentNode) (neighbors currentNode)
        queue' = queue ++ filter (not . (`inList` queue)) nbors
        nextNode = head $ qsort queue'
        queue'' = tail queue'
        visited' = visited ++ [currentNode]

