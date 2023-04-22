{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant id" #-}
module Main where


import Graphics.Gloss
--import GraphSearch
import Graphics.Gloss.Interface.IO.Interact (Event)
import Graph
import Prelude hiding (id)

-- Constants
windowWidth, windowHeight :: Int
windowWidth = 640
windowHeight = 480

-- Create the window
window :: Display
window = InWindow "Graph" (windowWidth, windowHeight) (10, 10)

drawNodes :: Graph -> [Picture]
drawNodes g = [uncurry translate (pos n) (thickCircle 20 4) | n <- nodes g]

-- drawEdges :: Graph -> [Picture]
-- drawEdges g = [x | n <- nodes g, x <- lns n]

drawEdges' :: Graph -> [Picture]
drawEdges' g = [x | n <- nodes g, x <- lns' n g]


-- drawGraph :: [Picture]
-- drawGraph = drawNodes graph <> drawEdges graph

drawGraph' :: [Picture]
drawGraph' = drawNodes g <> drawEdges' g

drawStep :: Int -> [Node] -> [Picture]
drawStep i n =
        let ns = take i n
            circles = [color red (uncurry translate (pos x) (circleSolid 20)) | x <- ns]
        in circles

-- lns :: Node -> [Picture]
-- lns n = if null (children n)
--         then []
--         else [line [pos n, pos x] | x <- children n]

lns' :: Node -> Graph -> [Picture]
lns' n g = do
                let edg = edges g !! (id n)
                if null edg
                then []
                else [line [pos n, pos (nodes g !! x)] | x <- edg]

-- selectSearch :: String -> [Node]
-- selectSearch s = case s of
--         "DFS" -> dfs graph a [] []
--         "BFS" -> bfs graph a [] []
--         _ -> []

-- used code from this lecture as a starting point to do animations
-- https://fosdem.org/2023/schedule/event/haskell_2d_animations/
data Model = Model
        { step     :: Int,
          complete :: Bool}

-- handleDisplay :: Model -> [Node] -> Picture
-- handleDisplay model lst = pictures (drawGraph <> drawStep (step model) lst)
--search = dfs graph a [] []
bfssearch = x
dfssearch = y
 
handleDisplay :: Model -> Picture
handleDisplay model = pictures (drawGraph' <> drawStep (step model) bfssearch)

handleEvent :: Event -> Model -> Model
handleEvent event model = model

handleTime :: Float -> Model -> Model
handleTime time (Model step True) = Model step True
handleTime time (Model step state) =
        let step' = step + 1
        --     state' = state || (step' == length (nodes graph))
            state' = state || (step' == length ns)
        in Model step' state'



-- Main function to display the window
main :: IO ()
main = do
        let model = Model 1 False
            fps = 2
        play window white fps model handleDisplay handleEvent handleTime

