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
-}
bfs :: Graph -> Node -> [Int] -> State [Node] [Node]
bfs graph start queue = do
    -- mark the starting node as visited
    modify (start:)
    -- enqueue the neighbors
    let neighbors = edges graph !! id start
        queue' = queue ++ filter (`notElem` queue) neighbors
    -- continue until the queue is empty
    case queue' of
        [] -> get
        (x:xs) -> do
            -- dequeue the next node
            --let neighbors = edges graph !! x
            --    unvisited = [nodes graph !! n | n <- filter (`notElem` xs) neighbors]
            -- mark each unvisited neighbor as visited
            --modify (reverse unvisited ++)

            -- enqueue each unvisited neighbor        
            bfs graph (nodes graph !! x) xs

ns = [Node 0 (-200, 0), Node 1 (-100, 50), Node 2 (0, 100), Node 3 (-100, -50)]
g = Graph [[1, 3], [2, 3], [], []] ns

x = reverse $ execState (bfs g (head ns) []) []

y = bfs g (head ns)