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
drawNodes g = [uncurry translate (pos n) (thickCircle 10 2) | n <- nodes g]

{- 
        Helper function for drawGraph
        Takes a graph and outputs a list of pictures of each edge
-}
drawEdges :: Graph -> [Picture]
drawEdges g = [x | n <- nodes g, x <- lns n g]

-- Creates a list of pictures of all nodes and edges of a graph
drawGraph :: Graph -> [Picture]
drawGraph g = drawNodes g <> drawEdges g

-- Draws the visited nodes in red for the current step in the visualization
drawStep :: Graph -> Int -> [Node] -> [Picture]
drawStep g i n =
        let ns = take i n
            circles = [color red (uncurry translate (pos x) (circleSolid 10)) | x <- ns]
            path = edgeStep g ns
        in circles  <> path

edgeStep :: Graph -> [Node] -> [Picture]
edgeStep g [] = []
edgeStep g ns = do

        currentN <- reverse ns
        let idx = fromMaybe (-1) $ elemIndex (id currentN) (map id ns)
        prevN <- reverse $ drop (idx + 1) ns
        let x | id prevN `elem` edges g !! id currentN = prevN
              | currentN == currentN = currentN

        [color red (line [pos currentN, pos (nodes g !! id x)])]



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

bfssearch = x
dfssearch = y

-- Takes a model and draws pictures on the screen according to the model
-- CURRENTLY JUST USED TO DISPLAY BFS-SEARCH
handleDisplay :: Graph -> [Node] -> Model -> Picture
handleDisplay g n model = pictures (drawGraph g <> drawStep g (step model) n)

{-
-- Takes a model and draws pictures on the screen according to the model
-- CURRENTLY JUST USED TO DISPLAY DFS-SEARCH
handleDisplay' :: Graph -> Model -> Picture
handleDisplay' g model = pictures (drawGraph <> drawStep g (step model) dfssearch)
-}

-- Needed by the play function but this has no real effect
handleEvent :: Event -> Model -> Model
handleEvent event model = model

-- Updates the time of the animation
handleTime :: Float -> Model -> Model
handleTime time (Model step True) = Model step True
handleTime time (Model step state) =
        let step' = step + 1
        --     state' = state || (step' == length (nodes graph))
            state' = state || (step' == length ns)
        in Model step' state'



randomPositions :: Int -> IO [(Float, Float)] -> IO [(Float, Float)]
randomPositions i l = do
        lst <- l
        if  i == length lst
        then return $ reverse lst
        else do
                x <- randomRIO (-300, 300) :: IO Float
                y <- randomRIO (-220, 220) :: IO Float
                randomPositions i (return ((x, y) : lst))


generateNodes :: Int -> IO [(Float, Float)] -> IO [Node]
generateNodes n lst = do
        pos <- lst
        return [Node i (pos !! i) | i <- range (0, n)]


generateEdges :: Int -> IO [[Int]]
generateEdges n = return [[x] | x <- range (0, n)]


generateGraph :: Int -> IO Graph
generateGraph n = do
        let ps = randomPositions (n+1) (return [])
        ns <- generateNodes n ps
        edgs <- generateRandomLists n
        return (Graph edgs ns)


randomList :: Int -> IO [Int]
randomList n = do
  len <- randomRIO (0, n) -- Randomly choose a length between 1 and 10
  lst <- replicateM len $ randomRIO (0, n) -- Generate a list of `len` random integers between 0 and 100
  return (sort $ nub lst)

generateRandomLists :: Int -> IO [[Int]]
generateRandomLists n = replicateM (n+1) $ randomList n

{- 
        Main function to display the window
        Creates a Model and sets the framerate

        User is then prompted to choose which alogirthm to run
-}

main :: IO ()
main = do
        --let m = [] :: [(Float, Float)]
        --n <- randomPosition 100 (return m)
        g <- generateGraph 10
        putStrLn "Nodes: "
        print $ nodes g
        putStrLn "Edges: "
        print $ edges g
        let model = Model 1 False
            fps = 1
            n = nodes g
            bfs' = reverse $ execState (bfs g (head n) []) []
            dfs' = reverse $ execState (dfs g (head n) []) []

        putStrLn "Choose algorithm: (bfs / dfs)"
        strIn <- getLine
        let str = [toLower x | x <- strIn]
        case str of
                "bfs" -> play window white fps model (handleDisplay g bfs') handleEvent handleTime
                "dfs" -> play window white fps model (handleDisplay g dfs') handleEvent handleTime
                _ -> putStrLn (str ++ " is not a valid algorithm")


