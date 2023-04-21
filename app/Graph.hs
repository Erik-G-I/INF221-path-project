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
bfs :: Graph -> Node -> State [Node] [Node]
bfs graph start = do
    -- enqueue the starting node
    let queue = [start]
    -- mark the starting node as visited
    modify (start:)
    -- continue until the queue is empty
    case queue of
        [] -> get
        (x:xs) -> do
            -- dequeue the next node
            let neighborIds = edges graph !! id x
                neighbors = [nodes graph !! n | n <- neighborIds]
                unvisited = filter (`notElem` xs) neighbors
            -- mark each unvisited neighbor as visited
            if not $ null unvisited
                then do
                    modify (reverse unvisited ++)
                    -- enqueue each unvisited neighbor        
                    bfs graph (head unvisited) >>= (\rest -> return $ x:rest)
                else get

ns = [Node 0 (0,0), Node 1 (1,1), Node 2 (2, 2), Node 3 (3, 3)]
g = Graph [[1, 3], [2], [], []] ns

x = runState (bfs g (head ns)) []
