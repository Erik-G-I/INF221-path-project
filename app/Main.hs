module Main where


import Graphics.Gloss
import BFS

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

g = drawNodes graph

lns :: Node -> [Picture]
lns n = if null (children n)
        then []
        else [line [pos n, pos x] | x <- children n]

{-
gridSize :: Int
gridSize = 100

walls :: [Picture]
walls = [polygon [(200, 300), (300, 300), (300, 200), (200, 200)], 
                polygon [(200, 200), (300, 200), (300, 100), (200, 100)]]

-- Create the grid
grid :: Picture
grid = pictures [ line [(x, 0), (x, fromIntegral windowHeight)] |
                x <- [0, fromIntegral gridSize .. fromIntegral windowWidth] ]
    <> pictures [ line [(0, y), (fromIntegral windowWidth, y)] |
                y <- [0, fromIntegral gridSize .. fromIntegral windowHeight] ]
    <> pictures walls

-- Align the picture to the window
align :: Picture
align = translate (-(fromIntegral windowWidth / 2)) (-(fromIntegral windowHeight / 2)) grid
-}


-- Main function to display the window
main :: IO ()
main = do
    display window white (scale 0.5 0.5 (pictures (drawNodes graph ++ drawEdges graph)))