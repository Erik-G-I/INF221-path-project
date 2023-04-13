module Main where


import Graphics.Gloss
import BFS
import Graphics.Gloss.Interface.IO.Interact (Event)

-- Constants
windowWidth, windowHeight :: Int
windowWidth = 640
windowHeight = 480

-- Create the window
window :: Display
window = InWindow "Graph" (windowWidth, windowHeight) (10, 10)

drawNodes :: Graph -> [Picture]
drawNodes g = [uncurry translate (pos n) (thickCircle 20 4) | n <- nodes g]

drawEdges :: Graph -> [Picture]
drawEdges g = [x | n <- nodes g, x <- lns n]

drawStep :: Int -> [Node] -> Picture
drawStep i n =
        let     m = n !! i
                p = pos m
                circle = uncurry translate p (circleSolid 20)
        in circle

g = drawNodes graph

lns :: Node -> [Picture]
lns n = if null (children n)
        then []
        else [line [pos n, pos x] | x <- children n]

-- testing animation using code from this lecture
-- https://fosdem.org/2023/schedule/event/haskell_2d_animations/
data Model = Model
        { step     :: Int,
          complete :: Bool}

handleDisplay :: Model -> Picture
handleDisplay model = pictures (drawEdges graph <> [drawStep (step model) (bfs graph a [] [])])

handleEvent :: Event -> Model -> Model
handleEvent event model = model

handleTime :: Float -> Model -> Model
handleTime time (Model step state) =
        let step' = step + 1
            state' = state || (step' == length (nodes graph))
        in Model step' state



-- Main function to display the window
main :: IO ()
main = do
        let model = Model 0 False
        play window white 1 model handleDisplay handleEvent handleTime
    --display window white (scale 0.5 0.5 (pictures (drawNodes graph ++ drawEdges graph)))