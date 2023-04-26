{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant id" #-}
module WeightedGraph where
import Control.Monad.Trans.State.Lazy
import Prelude hiding (id)

-- Node has an id and a postition
data Node = Node {id :: Int, pos :: (Float, Float)}
    deriving (Eq)

instance Show Node where
    show = show . id

-- edges is list of list of tuples with the weight of the edge and the id of the node
-- nodes is a list of all nodes
data Graph = Graph {edges :: [[(Int, Int)]], nodes :: [Node]}


{-
    Dijkstra using state-monad to keep track of the lowest cost path

    graph - The graph that the algorithm will run on
    currentNode - The starting node in the graph where the algoritm will start from
    endNode - The node we want to reach
    queue - list of the next edges to be explored [(Weight, Node id)]
-}
dijkstra :: Graph -> Node -> Node -> [(Int, Int)] -> State (Int, [Node]) (Int, [Node])
dijkstra graph currentNode endNode queue = do
    if currentNode == endNode
    then get
    else do 
        let neighbors = edges graph !! id currentNode
            
        return undefined