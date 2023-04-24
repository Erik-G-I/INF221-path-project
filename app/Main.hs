{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant id" #-}
module Main where


import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact (Event)
import Graph
import Prelude hiding (id)
import Data.Char (toLower)

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
drawNodes g = [uncurry translate (pos n) (thickCircle 20 4) | n <- nodes g]

{- 
        Helper function for drawGraph
        Takes a graph and outputs a list of pictures of each edge
-}
drawEdges :: Graph -> [Picture]
drawEdges g = [x | n <- nodes g, x <- lns n g]

-- Creates a list of pictures of all nodes and edges of a graph
drawGraph :: [Picture]
drawGraph = drawNodes g <> drawEdges g

-- Draws the visited nodes in red for the current step in the visualization
drawStep :: Int -> [Node] -> [Picture]
drawStep i n =
        let ns = take i n
            circles = [color red (uncurry translate (pos x) (circleSolid 20)) | x <- ns]
        in circles

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
handleDisplay :: Model -> Picture
handleDisplay model = pictures (drawGraph <> drawStep (step model) bfssearch)

-- Takes a model and draws pictures on the screen according to the model
-- CURRENTLY JUST USED TO DISPLAY DFS-SEARCH
handleDisplay' :: Model -> Picture
handleDisplay' model = pictures (drawGraph <> drawStep (step model) dfssearch)

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

{- 
        Main function to display the window
        Creates a Model and sets the framerate

        User is then prompted to choose which alogirthm to run
-}
main :: IO ()
main = do
        let model = Model 1 False
            fps = 2

        putStrLn "Choose algorithm: (bfs / dfs)"
        strIn <- getLine
        let str = [toLower x | x <- strIn]
        case str of
                "bfs" -> play window white fps model handleDisplay handleEvent handleTime
                "dfs" -> play window white fps model handleDisplay' handleEvent handleTime
                _ -> putStrLn (str ++ " is not a valid algorithm")
        
        
