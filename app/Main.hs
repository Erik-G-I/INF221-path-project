{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant id" #-}
module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact (Event)
import Graph
import Prelude hiding (id)
import Data.Char (toLower)
import Data.Ix (Ix(range))
import System.Random
import Data.Maybe (fromMaybe)
import Data.List (elemIndex, nub, sort)
import Control.Monad.State

-- Constants
windowWidth, windowHeight :: Int
windowWidth = 640
windowHeight = 480

-- Create the window
window :: Display
window = InWindow "Graph" (windowWidth, windowHeight) (10, 10)

{-
        Helper function for drawGraph
        Takes a graph and outputs a list of pictures of each node
-}
drawNodes :: Graph -> [Picture]
drawNodes g = [uncurry translate (pos n) $ pictures [color white (thickCircle 10 2), color green $ scale 0.2 0.2 $ text (show $ id n)] | n <- nodes g]

{- 
        Helper function for drawGraph
        Takes a graph and outputs a list of pictures of each edge
-}
drawEdges :: Graph -> [Picture]
drawEdges g = [color white x | n <- nodes g, x <- lns n g]

-- Creates a list of pictures of all nodes and edges of a graph
drawGraph :: Graph -> [Picture]
drawGraph g = drawNodes g <> drawEdges g

{-
        Draws the visited nodes in red for the current step in the bfs visualization
        does not highlight edges at each step :((
-}
bfsDrawStep :: Graph -> Int -> [Node] -> [Picture]
bfsDrawStep g i n =
        let ns = take i n
            circles = [uncurry translate (pos x) $ pictures [color red (circleSolid 10), color green $ scale 0.2 0.2 $ text (show $ id x)] | x <- ns]
            --path = edgeStep "BFS" g ns
        in circles -- <> path

-- Draws the visited nodes in red for the current step in the dfs visualization
dfsDrawStep :: Graph -> Int -> [Node] -> [Picture]
dfsDrawStep g i n =
        let ns = take i n
            circles = [uncurry translate (pos x) $ pictures [color red (circleSolid 10), color green $ scale 0.2 0.2 $ text (show $ id x)] | x <- ns]
            path = edgeStep "DFS" g ns
        in path <> circles

edgeStep :: String -> Graph -> [Node] -> [Picture]
edgeStep _ g [] = []
edgeStep s g ns = case s of
        "BFS" -> [color red (line [pos (last ns), pos $ bfsEdgeCheck g (head ns:[n])])| n <- tail ns] ++ edgeStep "BFS" g (tail ns)
        "DFS" -> color red (line [pos (last ns), pos $ dfsEdgeCheck g (reverse ns)]) : edgeStep "DFS" g (init ns)

{-
        not working properly
        could not manage to come up with a strategy to color the correct edges in bfs
-} 
bfsEdgeCheck :: Graph -> [Node] -> Node
bfsEdgeCheck _ [n] = n
bfsEdgeCheck g (n:m:ms) | id n `elem` edges g !! id m = m
                        | otherwise = bfsEdgeCheck g (m:ms)

-- finds and returns the first node with an edge to the current node n
dfsEdgeCheck :: Graph -> [Node] -> Node
dfsEdgeCheck _ [n] = n
dfsEdgeCheck g (n:m:ms) | id n `elem` edges g !! id m = m
                        | otherwise = dfsEdgeCheck g (n:ms)

{-
        Helper function for drawEdges
        Checks for edges of a node. If it has any edges it creates a list of lines to each neighbor
-}
lns :: Node -> Graph -> [Picture]
lns n g = do
                let edg = edges g !! id n
                if null edg
                then []
                else [line [pos n, pos (nodes g !! x)] | x <- edg]


{-
        used code from this lecture as a starting point to do animations
        https://fosdem.org/2023/schedule/event/haskell_2d_animations/

        Create a constructor for a model
        Step is a counter used to draw the current picture on the screen
        Complete is a bool to keep track if the animation has finished
-}
data Model = Model
        { step     :: Int,
          complete :: Bool}


-- Takes a graph, node-list and model and draws pictures on the screen according to the model
handleDisplay :: String -> Graph -> [Node] -> Model -> Picture
handleDisplay s g n model = case s of 
        "BFS" -> pictures (drawGraph g <> bfsDrawStep g (step model) n)
        "DFS" -> pictures (drawGraph g <> dfsDrawStep g (step model) n)

-- Needed by the play function but this has no real effect
handleEvent :: Event -> Model -> Model
handleEvent event model = model

-- Updates the time of the animation
handleTime :: [Node] -> Float -> Model -> Model
handleTime ns time (Model step True) = Model step True
handleTime ns time (Model step state) =
        let step' = step + 1
        --     state' = state || (step' == length (nodes graph))
            state' = state || (step' == length ns)
        in Model step' state'


{-
        i - size of list we want to generate
        l - empty list to start and later used in recursion
-}
randomPositions :: Int -> IO [(Float, Float)] -> IO [(Float, Float)]
randomPositions n l = do
        lst <- l
        if  n == length lst
        then return $ reverse lst
        else do
                x <- randomRIO (-300, 300) :: IO Float
                y <- randomRIO (-220, 220) :: IO Float
                randomPositions n (return ((x, y) : lst))
        
{-
        n - how many nodes we want
        lst - list of positions
-}
generateNodes :: Int -> IO [(Float, Float)] -> IO [Node]
generateNodes n lst = do
        pos <- lst
        return [Node n' (pos !! n') | n' <- range (0, n)]


{-
        n - how many nodes the graph should contain
-}
generateGraph :: Int -> IO Graph
generateGraph n = do
        let ps = randomPositions (n+1) (return [])
        ns <- generateNodes n ps
        edgs <- generateRandomLists n
        return (Graph edgs ns)


{-
        generates a list of random length between 0 and n 
        fills list of random numbers between 0 and n
        removes duplicate Ints and sorts for better readability 
-}
randomList :: Int -> IO [Int]
randomList n = do
  len <- randomRIO (0, n)
  lst <- replicateM len $ randomRIO (0, n)
  return (sort $ nub lst)

{-
        runs randomList function n times to create an adjacency list for all n nodes
-}
generateRandomLists :: Int -> IO [[Int]]
generateRandomLists n = replicateM (n+1) $ randomList n

{- 
        Main function to display the window
        Creates a Model and sets the framerate

        User is prompted to choose the size of the generated graph
        as well as which alogirthm to run on the graph

        the list of nodes and edges is also printed in the terminal

-}
main :: IO ()
main = do
        putStrLn "Input the size of graph you want to generate: "
        size <- getLine
        --let m = [] :: [(Float, Float)]
        --n <- randomPosition 100 (return m)
        g <- generateGraph (read size)
        putStr "Generated random graph of size: "
        print size
        putStrLn "Nodes: "
        print $ nodes g
        putStrLn "Edges: "
        print $ edges g
        let model = Model 1 False
            fps = 2 -- can be set to desired speed. I find 2 to be best for graphs of size 10 and up. 1 is good for smaller graphs
            n = nodes g
            bfs' = reverse $ execState (bfs g (head n) []) []
            dfs' = reverse $ execState (dfs g (head n) []) []

        putStrLn "Choose algorithm: (bfs / dfs)"
        strIn <- getLine
        let str = [toLower x | x <- strIn]
        case str of
                "bfs" -> do 
                        putStr "bfs: "
                        print bfs'
                        play window black fps model (handleDisplay "BFS" g bfs') handleEvent (handleTime n)
                "dfs" -> do 
                        putStr "dfs: "
                        print dfs'
                        play window black fps model (handleDisplay "DFS" g dfs') handleEvent (handleTime n)
                _ -> putStrLn (str ++ " is not a valid algorithm")

