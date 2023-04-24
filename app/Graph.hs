{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant id" #-}
module Graph where
import Control.Monad.Trans.State.Lazy
import Prelude hiding (id)

-- Node has an id and a postition
data Node = Node {id :: Int, pos :: (Float, Float)}
    deriving (Eq)

instance Show Node where
    show = show . id

-- edges is an adjacency list for each node with id = index
-- nodes is a list of all nodes
data Graph = Graph {edges :: [[Int]], nodes :: [Node]}


{-
    BFS search using state-monad to keep track of visited nodes

    Graph - The graph that the algorithm will run on
    Node - The starting node in the graph where the algoritm will start from
    [Int] - Queue of the next nodes to be explored
-}
bfs :: Graph -> Node -> [Int] -> State [Node] [Node]
bfs graph start queue = do
    visited <- get
    -- mark the starting node as visited
    modify (start:)
    -- enqueue the neighbors
    let neighbors = edges graph !! id start
        queue' = queue ++ filter (`notElem` (queue ++ [id v | v <- visited])) neighbors
    -- continue until the queue is empty
    case queue' of
        [] -> get
        (x:xs) -> do
            bfs graph (nodes graph !! x) xs


{-
    DFS search using state-monad to keep track of visited nodes

    Graph - The graph that the algorithm will run on
    Node - The starting node in the graph where the algoritm will start from
    [Int] - Queue of the next nodes to be explored
-}
dfs :: Graph -> Node -> [Int] -> State [Node] [Node]
dfs graph start queue = do
    visited <- get
    -- mark the starting node as visited
    modify (start:)
    -- enqueue the neighbors
    let neighbors = edges graph !! id start
        queue' = filter (\n -> (nodes graph !! n) `notElem` visited) neighbors ++ queue
    -- continue until the queue is empty
    case queue' of
        [] -> get
        (x:xs) -> do
            dfs graph (nodes graph !! x) xs



-- manual implementation of some graphs until i can generate graphs automatically
ns' = [Node 0 (-200, 0), Node 1 (-100, 50), Node 2 (0, 100), Node 3 (-100, -50)]
ns = [Node 0 (-200, 0), 
      Node 1 (-100, 50), 
      Node 2 (-100, 0), 
      Node 3 (-100, -50), 
      Node 4 (0, 0), 
      Node 5 (50, 25), 
      Node 6 (50, -25), 
      Node 7 (-50, -25), 
      Node 8 (-50, -75), 
      Node 9 (-50, 100), 
      Node 10 (0, 150)]

g' = Graph [[1, 3], [2, 3], [], []] ns'
g = Graph [[1, 2, 3], [9], [4], [7, 8], [5, 6], [], [], [], [], [10, 4], []] ns

x = reverse $ execState (bfs g (head ns) []) []
y = reverse $ execState (dfs g (head ns) []) []

