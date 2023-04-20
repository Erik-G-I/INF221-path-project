module Main where


import Graphics.Gloss
import GraphSearch
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

drawGraph :: [Picture]
drawGraph = drawNodes graph <> drawEdges graph

drawStep :: Int -> [Node] -> [Picture]
drawStep i n =
        let ns = take i n
            circles = [color red (uncurry translate (pos x) (circleSolid 20)) | x <- ns]
        in circles

lns :: Node -> [Picture]
lns n = if null (children n)
        then []
        else [line [pos n, pos x] | x <- children n]

selectSearch :: String -> [Node]
selectSearch s = case s of
        "DFS" -> dfs graph a [] []
        "BFS" -> bfs graph a [] []
        _ -> []

-- used code from this lecture as a starting point to do animations
-- https://fosdem.org/2023/schedule/event/haskell_2d_animations/
data Model = Model
        { step     :: Int,
          complete :: Bool}

-- handleDisplay :: Model -> [Node] -> Picture
-- handleDisplay model lst = pictures (drawGraph <> drawStep (step model) lst)
search = bfs graph a [] []

handleDisplay :: Model -> Picture
handleDisplay model = pictures (drawGraph <> drawStep (step model) search)

handleEvent :: Event -> Model -> Model
handleEvent event model = model

handleTime :: Float -> Model -> Model
handleTime time (Model step True) = Model step True
handleTime time (Model step state) =
        let step' = step + 1
            state' = state || (step' == length (nodes graph))
        in Model step' state'



-- Main function to display the window
main :: IO ()
main = do
        -- putStrLn "Select search algorithm: (BFS, DFS)"
        -- x <- getContents
        -- let lst = selectSearch x
        -- let model = Model 1 False
        -- play window white 1 model (`handleDisplay` lst) handleEvent handleTime
        let model = Model 1 False
        play window white 1 model handleDisplay handleEvent handleTime