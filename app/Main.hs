module Main where


import Graphics.Gloss
import Algorithms



-- Constants
windowWidth, windowHeight :: Int
windowWidth = 400
windowHeight = 300

gridSize :: Int
gridSize = 100

-- Create the window
window :: Display
window = InWindow "Grid" (windowWidth, windowHeight) (10, 10)

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

-- toGrid :: [String] -> Grid
-- toGrid l = 

-- getGrid :: Grid
-- getGrid = do
--     lns <- readFile "grid.txt"
--     let lst = lines lns
--    toGrid lst

-- Main function to display the window and grid
main :: IO ()
main = display window white align


