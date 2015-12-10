{-|
    Module      : Sokoban
    Description : Sokoban project for MIEI LI1 - Final game
    License     : Unlicensed
-}

module Main where

import Data.IORef
import Data.Maybe
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Text as T
import Data.Char
import Data.List
import Data.List.Split
import Debug.Trace

import Graphics.Gloss
import Graphics.Gloss.Data.Picture -- To draw pictures
import Graphics.Gloss.Interface.Pure.Game -- To react to events
import Graphics.Gloss.Juicy -- Extra module to import images in formats other than bitmap

import Audio -- Our custom audio library (from stack overflow)

-- The World type, used to hold the game state
-- Note: All Y coordinates used in the code are relative to the top of the map (except they're converted to gloss coordinates by the render function)
data World = World {
              mapLines :: [String], -- A list of map lines
              playerPos :: [Int],   -- A list containing the player's position in the format [x, y]
              boxPos :: [[Int]]     -- A list containing all the box positions in the format [x, y]
             } deriving (Show)

-- Global constants
windowWidth = 1024 :: Int
windowHeight = 768 :: Int
mapWidth = 40 :: Int -- Map size in characters, used to calculate sprite positions
mapHeight = 30 :: Int
spriteWidth = (fromIntegral windowWidth) / (fromIntegral mapWidth) :: Float -- The size sprites should be displayed at on the window
spriteHeight = (fromIntegral windowHeight) / (fromIntegral mapHeight) :: Float
bmpWidth = 128 :: Float  -- The size the bmp files have (used to calculate the scale that they should be displayed at)
bmpHeight = 128 :: Float -- It's important that all used .bmp files have exactly this size in pixels

-- | Main function which starts off the game
main :: IO ()
main = play (InWindow "Sokoban" (windowWidth, windowHeight) (100, 100)) white 60 initialWorld drawWorld handleInput updateWorld
    where
        initialWorld = World
                     ["                                        ",
                      "                                        ",
                      "                                        ",
                      "                                        ",
                      "                                        ",
                      "                                        ",
                      "                                        ",
                      "                                        ",
                      "                                        ",
                      "               #####                    ",
                      "               #   #                    ",
                      "               #   #                    ",
                      "             ###   ##                   ",
                      "             #      #                   ",
                      "           ### # ## #   ######          ",
                      "           #   # ## #####  ..#          ",
                      "           #               ..#          ",
                      "           ##### ### # ##  ..#          ",
                      "               #     #########          ",
                      "               #######                  ",
                      "                                        ",
                      "                                        ",
                      "                                        ",
                      "                                        ",
                      "                                        ",
                      "                                        ",
                      "                                        ",
                      "                                        ",
                      "                                        ",
                      "                                        "]
                      [22, 17]
                      [[16, 11],
                      [18, 12],
                      [16, 13],
                      [18, 13],
                      [13, 16],
                      [16, 16]]

-- | A function that converts the world into a Picture
drawWorld :: World -> Picture
drawWorld world = pictures (drawnMap ++ drawnBoxes ++ [drawnPlayer])
    where
        -- Sprites

        prePlayerSprite = case (unsafePerformIO $ loadJuicyPNG "./assets/Character4.png") of
                            Just sprite -> sprite
                            Nothing -> Color black $ rectangleSolid 128 128
        playerSprite = Scale (spriteWidth / bmpWidth) (spriteHeight / bmpHeight) prePlayerSprite

        preBoxSprite = case (unsafePerformIO $ loadJuicyPNG "./assets/Crate_Brown.png") of
                         Just sprite -> sprite
                         Nothing -> Color red $ rectangleSolid 128 128
        boxSprite = Scale (spriteWidth / bmpWidth) (spriteHeight / bmpHeight) preBoxSprite

        -- Sprite that represents a box which has already been placed on one of the designated spots
        preBlueBoxSprite = case (unsafePerformIO $ loadJuicyPNG "./assets/Crate_Blue.png") of
                             Just sprite -> sprite
                             Nothing -> Color blue $ rectangleSolid 128 128
        placedboxSprite = Scale (spriteWidth / bmpWidth) (spriteHeight / bmpHeight) preBlueBoxSprite

        preGroundSprite = case (unsafePerformIO $ loadJuicyPNG "./assets/GroundGravel_Concrete.png") of
                            Just sprite -> sprite
                            Nothing -> Color blue $ rectangleSolid 128 128
        groundSprite = Scale (spriteWidth / bmpWidth) (spriteHeight / bmpHeight) preGroundSprite

        -- Sprite that represents a place on the ground where a box should be placed
        preBoxGroundSprite = case (unsafePerformIO $ loadJuicyPNG "./assets/GroundGravel_Grass.png") of
                               Just sprite -> sprite
                               Nothing -> Color blue $ rectangleSolid 128 128
        boxGroundSprite = Scale (spriteWidth / bmpWidth) (spriteHeight / bmpHeight) preBoxGroundSprite

        wallSprite = Scale (spriteWidth / bmpWidth) (spriteHeight / bmpHeight) (unsafePerformIO $ loadBMP "./assets/Wall_Beige.bmp")

        -- Drawn sprites
        drawnMap = drawMap world wallSprite groundSprite boxGroundSprite :: [Picture]
        drawnPlayer = drawPlayer world playerSprite :: Picture
        drawnBoxes = drawBoxes world boxSprite placedboxSprite :: [Picture]

-- | Creates the Picture objects for the map elements (walls and floor) and returns them in a list
drawMap :: World     -- ^ The world object
        -> Picture   -- ^ The wall sprite
        -> Picture   -- ^ The normal ground sprite
        -> Picture   -- ^ The sprite used to represent a ground block where a box should be placed
        -> [Picture] -- ^ Returns the list of rendered pictures
drawMap world wallSprite groundSprite boxGroundSprite = aux world 0 0
    where
        -- xPos and yPos refer to the current character of the map we are printing
        aux world xPos yPos
            | null (mapLines world) = []
            | otherwise = (drawMapLine x xPos yPos) ++ (aux (world {mapLines = xs}) 0 (yPos + 1))
                where
                    (x : xs) = mapLines world

        drawMapLine [] _ _ = []
        drawMapLine (x : xs) xPos yPos = (Translate displayX displayY spriteToUse) : (drawMapLine xs (xPos + 1) yPos)
            where
                spriteToUse = if (x == '#') then wallSprite else if (x == '.') then boxGroundSprite else if (x == ' ') then groundSprite else wallSprite
                [displayX, displayY] = coordinatesToGloss [(spriteWidth * xPos), (spriteHeight * yPos)]

-- | Creates the Picture object for the player (with the correct size and position)
drawPlayer :: World   -- ^ The world object
           -> Picture -- ^ The player sprite
           -> Picture -- ^ Returns the final Picture object for the player
drawPlayer world sprite = Translate displayX displayY  sprite
    where
        pPos = playerPos world
        playerX = fromIntegral (pPos !! 0) :: Float
        playerY = fromIntegral (pPos !! 1) :: Float
        [displayX, displayY] = coordinatesToGloss [(spriteWidth * playerX), (spriteHeight * playerY)]

-- | Creates the Picture objects for the boxes
drawBoxes :: World     -- ^ The world object
          -> Picture   -- ^ The not-yet-placed box sprite
          -> Picture   -- ^ The correctly-placed box sprite
          -> [Picture] -- ^ Returns the list of box Pictures
drawBoxes world boxSprite placedBoxSprite
    | null (boxPos world) = []
    | otherwise = (Translate displayX displayY spriteToUse) : (drawBoxes (world {boxPos = xs}) boxSprite placedBoxSprite)
        where
            (x : xs) = boxPos world
            boxX = x !! 0
            boxY = x !! 1
            charAtBoxPosition = (mapLines !! boxY) !! boxX
            isBoxPlaced = if (charAtBoxPosition == '.') then True else False
            spriteToUse = if (isBoxPlaced) then placedBoxSprite else boxSprite
            [displayX, displayY] = coordinatesToGloss [(spriteWidth * (fromIntegral boxX)), (spriteHeight * (fromIntegral boxY))]

-- | A function to handle input events
handleInput :: Event -> World -> World
handleInput (EventKey (SpecialKey KeyUp) Down _ _) world = movePlayer 'U' world
handleInput (EventKey (SpecialKey KeyDown) Down _ _) world = movePlayer 'D' world
handleInput (EventKey (SpecialKey KeyLeft) Down _ _) world = movePlayer 'L' world
handleInput (EventKey (SpecialKey KeyRight) Down _ _) world = movePlayer 'R' world
handleInput _ world = world

-- | A function to step the world one iteration. It is passed the period of time (in seconds) needing to be advanced
updateWorld :: Float -> World -> World
updateWorld deltaT world = world -- Quietly do nothing (me irl)

-- | Moves the player in the world based on a movement command
movePlayer :: Char -> World -> World
movePlayer command world@(World mapLines playerPos boxPos)
    | isCommandValid = world {playerPos = newPlayerPos, boxPos = newBoxPos}
    | otherwise = world
    where
        isCommandValid = validateCommand command mapLines boxPos playerPos
        (newPlayerPos : newBoxPos) = processCommand command mapLines boxPos playerPos

-- | Checks if a command is valid and returns a boolean accordingly
validateCommand :: Char     -- ^ The command to be processed (can be L (left) U (up) R (right) or D (down))
                -> [String] -- ^ The list of map lines
                -> [[Int]]  -- ^ The list of box coordinates in the format [[x1, y1], [x2, y2]] (y relative to top)
                -> [Int]    -- ^ The player coordinates (y relative to top)
                -> Bool     -- ^ Returns true if the command is valid, false otherwise
validateCommand command mapLines boxPos playerPos =

    -- Check that no coordinates are out of bounds
    if ((desiredX < 0 || desiredX > maxX) || (desiredY < 0 || desiredY > maxY)) then
        False
    -- Desired position is available
    else if (charAtDesiredPosition /= '#' && not (isBoxAtDesiredPosition)) then
        True
    -- Desired position is a wall
    else if (charAtDesiredPosition == '#') then
        False
    -- There is a box at the desired position
    else if (charAtDesiredPosition /= '#' && isBoxAtDesiredPosition) then
        -- Box has nowhere to go
        if (isBoxAheadOfDesiredPosition || (charAheadOfDesiredPosition == '#')) then
            False
        -- Box can move
        else
            True
    -- Default to false in case we missed anything above
    else
        False

    where
        maxX = (length (mapLines !! 0)) - 1
        maxY = (length (mapLines)) - 1
        cmdVector = commandToVector command
        desiredX = (fst cmdVector) + (playerPos !! 0)
        desiredY = (snd cmdVector) + (playerPos !! 1)
        afterDesiredX = desiredX + (fst cmdVector)
        afterDesiredY = desiredY + (snd cmdVector)
        -- What is there on the place we want to move to?
        charAtDesiredPosition = {- trace ("\ncharAtDesiredPosition: " ++ show ((mapLines !! desiredY) !! desiredX)) -} ((mapLines !! desiredY) !! desiredX)
        -- Is there a box at the place we want to move to?
        isBoxAtDesiredPosition = {- trace ("isBoxAtDesiredPosition: " ++ (show (elem [desiredX, desiredY] boxPos))) -} (elem [desiredX, desiredY] boxPos)
        -- What is there one position ahead of the place we want to move to? (necessary if, for example, there is a box in the place we want to move to)
        charAheadOfDesiredPosition = {- trace ("CharAheadOfDesiredPosition: " ++ (show ((mapLines !! afterDesiredY) !! afterDesiredY))) -} ((mapLines !! afterDesiredY) !! afterDesiredX)
        -- Is there a box one position ahead of the place we want to move to?
        isBoxAheadOfDesiredPosition = {- trace ("isBoxAheadOfDesiredPosition: " ++ show (elem [afterDesiredX, afterDesiredY] boxPos)) -} (elem [afterDesiredX, afterDesiredY] boxPos)

-- | Processes a command on a map, a player coordinate, and a set of box coordinates, and returns the resulting player and box coordinates
processCommand :: Char     -- ^ The command to be processed (can be L (left) U (up) R (right) or D (down))
               -> [String] -- ^ The list of map lines
               -> [[Int]]  -- ^ The list of box coordinates in the format [[x1, y1], [x2, y2]] (y relative to top)
               -> [Int]    -- ^ The player coordinates (y relative to top)
               -> [[Int]]  -- ^ Returns a list of coordinates, with the first being the player position the the remaining ones being box positions
processCommand command mapLines boxPos playerPos =

    -- There isn't a box at the desired position (we do not need to move a box)
    if (not (isBoxAtDesiredPosition)) then
        [desiredX, desiredY] : boxPos

    -- There is a box at our desired position, so we have to move it
    else
        let
            newBoxPos = [afterDesiredX, afterDesiredY]
        in
            [desiredX, desiredY] : newBoxPos : (delete [desiredX, desiredY] boxPos)

    where
        cmdVector = commandToVector command
        desiredX = (fst cmdVector) + (playerPos !! 0)
        desiredY = (snd cmdVector) + (playerPos !! 1)
        afterDesiredX = desiredX + (fst cmdVector)
        afterDesiredY = desiredY + (snd cmdVector)
        -- What is there on the place we want to move to?
        charAtDesiredPosition = (mapLines !! desiredY) !! desiredX
        -- Is there a box at the place we want to move to?
        isBoxAtDesiredPosition = elem [desiredX, desiredY] boxPos

-- | Checks a map and player and box coordinates and returns true if the game has been completed, false otherwise
isGameCompleted :: [String] -- ^ The list of map lines
                -> [[Int]]  -- ^ The list of box coordinates in the format [[x1, y1], [x2, y2]] (y relative to top)
                -> [Int]    -- ^ The player coordinates (y relative to top)
                -> Bool     -- ^ Returns either "FIM" or "INCOMPLETO" based on game status
isGameCompleted mapLines boxPos playerPos =
    if (allBoxesPlaced mapLines boxPos) then
        True
    else
        False
    where
        allBoxesPlaced mapLines [] = True
        allBoxesPlaced mapLines (x : xs) =
            let
                xPos = x !! 0
                yPos = x !! 1
            in
                if (((mapLines !! yPos) !! xPos) == '.') then
                    allBoxesPlaced mapLines xs
                else
                    False

{-
    Auxiliary functions
-}

-- | Converts coordinates to the corresponding values relative to the game window (used with Translate).
-- Needed because gloss' coordinates are relative to the center of the window and ours are relative to the top left
coordinatesToGloss :: [Float] -- ^ The coordinates to be converted (Notice: should be in pixels, not in map units)
                   -> [Float] -- ^ The resulting coordinates ready to be used with Translate
coordinatesToGloss coordinates = [convertedX, convertedY]
  where
    x = coordinates !! 0
    y = coordinates !! 1
    -- When we convert the coordinates, we have to add half of the sprite's width and height in order to center it at that coordinate
    convertedX = (x - ((fromIntegral windowWidth) / 2) + (spriteWidth / 2))
    convertedY = -(y - ((fromIntegral windowHeight) / 2) + (spriteHeight / 2))

-- | Parses a list of coordinate lines to the format [[x1, y1], [x2, y2]]
parseCoordinateLines :: [String] -- ^ The list of coordinate lines to be parsed
                     -> [[Int]]  -- ^ Returns the received coordinates as [[x1, y1], [x2, y2]]
parseCoordinateLines [] = []
parseCoordinateLines (x : xs) = let [a, b] = splitOn " " x in [read a, read b] : parseCoordinateLines xs

-- | Converts a set of coordinates to a coordinate string
coordinatesToString :: Int    -- ^ The x coordinate
                    -> Int    -- ^ The y coordinate
                    -> String -- ^ Returns the coordinate string
coordinatesToString xCoord yCoord = (show xCoord) ++ " " ++ (show yCoord)

-- | Converts a command to the corresponding movement vector. For example, calling this function with 'U' yields (0, 1)
commandToVector :: Char       -- ^ The command to be converted
                -> (Int, Int) -- ^ The resulting vector
commandToVector x
    | (x == 'U') || (x == 'u') = (0, -1) -- Up and down are inverted here because we deal with Y coordinates as top-relative
    | (x == 'D') || (x == 'd') = (0, 1)
    | (x == 'R') || (x == 'r') = (1, 0)
    | (x == 'L') || (x == 'l') = (-1, 0)
    | otherwise = (0, 0)

-- | Converts a Y coordinate to become relative to the bottom of the map
yCoordinateToBottomRelative :: Int -- ^ The Y coordinate
                            -> Int -- ^ The height of the map
                            -> Int -- ^ The new, converted Y coordinate
yCoordinateToBottomRelative yCoord mapHeight =  (mapHeight - 1) - yCoord

-- | Converts a Y coordinate to become relative to the top of the map
yCoordinateToTopRelative :: Int -- ^ The Y coordinate
                         -> Int -- ^ The height of the map
                         -> Int -- ^ The new, converted Y coordinate
yCoordinateToTopRelative yCoord mapHeight = (mapHeight - 1) - yCoord

-- | Converts a list of coordinates to having the Y coordinate relative to the bottom of the map
yCoordinatesToBottomRelative :: [[Int]] -- ^ The list of coordinates
                             -> Int     -- ^ The height of the map
                             -> [[Int]] -- ^ The new, converted list of coordinates
yCoordinatesToBottomRelative [] _ =  []
yCoordinatesToBottomRelative ([a, b] : xs) mapHeight = [a, ((mapHeight - 1) - b)] : (yCoordinatesToBottomRelative xs mapHeight)

-- | Converts a list of coordinates to having the Y coordinate relative to the top of the map
yCoordinatesToTopRelative :: [[Int]] -- ^ The list of coordinates
                          -> Int     -- ^ The height of the map
                          -> [[Int]] -- ^ The new, converted list of coordinates
yCoordinatesToTopRelative [] _ =  []
yCoordinatesToTopRelative ([a, b] : xs) mapHeight = [a, ((mapHeight - 1) - b)] : (yCoordinatesToTopRelative xs mapHeight)

-- | Checks if a list has only a certain set of items
listHasOnly :: (Eq a)
            => [a] -- ^ List to evaluate
            -> [a] -- ^ List of items the list can contain
            -> Bool
listHasOnly [] [] = True
listHasOnly [] _ = False
listHasOnly list items = not (elem False (map aux list))
    where
        aux x = aux2 x items
        aux2 x [] = False
        aux2 x (y : ys)
            | (x == y) = True
            | (x /= y && null ys) = False
            | otherwise = aux2 x ys

-- | Returns the index at which a list of strings stops having a certain head character
listHeadNot :: [String] -> Char -> Int
listHeadNot [] _ = 0
listHeadNot x char = aux x char 0
    where
        aux [] _ currentIndex = currentIndex
        aux (x : xs) char currentIndex
            | (length x < 1) = currentIndex
            | (head x /= char) = currentIndex
            | otherwise = aux xs char (currentIndex + 1)

-- | Returns the index at which the first character of a line from a list of strings is a certain character or the length of the list if the character is not found
listHeadIs :: [String] -> Char -> Int
listHeadIs [] _ = 0
listHeadIs x char = aux x char 0
    where
        aux [] _ currentIndex = currentIndex
        aux (x : xs) char currentIndex
            | (length x > 0 && head x == char) = currentIndex
            | (null xs) = currentIndex + 1
            | otherwise = aux xs char (currentIndex + 1)

-- | My own version of elemIndex that returns an Int all the time instead of a stupid maybe int that just makes code bloated. I hate haskell (shh).
-- Returns index at which an element is found in a list or \-1 if the element isn't found
myElemIndex :: (Eq a) => a -> [a] -> Int
myElemIndex x list = aux x list 0
    where
        aux _ [] _ = -1
        aux x (y : ys) index
            | x == y = index
            | otherwise = aux x ys (index + 1)

-- | Checks if a coordinate is in the right format (number, space, number) e.g. "12 24"
validateCoordinateFormat :: String -> Bool
validateCoordinateFormat line
    | length spaces /= 1 = False
    | length other > 0 = False
    | (myElemIndex ' ' line == 0) || (myElemIndex ' ' line == ((length line) - 1)) = False
    | length coordinates > 2 = False
    | otherwise = True
    where
        spaces = filter (== ' ') line
        other = filter (\x -> not (isDigit x) && not (x == ' ')) line
        coordinates = splitOn [' '] line

-- | Returns the number of ocurrences of a given element on a list of lists
howManyTimes :: (Eq a) => a -> [[a]] -> Int
howManyTimes x [] = 0
howManyTimes x (y : ys) = (aux x y) + howManyTimes x ys
    where
        aux :: (Eq a) => a -> [a] -> Int
        aux x [] = 0
        aux x (y : ys)
            | x == y = 1 + aux x ys
            | otherwise = aux x ys

-- | Checks the coordinates present in a list of coordinate lines are all different, and if not, returns the index of the first repeating coordinate
noRepeatedCoordinates :: [String] -> Int
noRepeatedCoordinates list = aux list 0 []
    where
        aux [] _ _ = -1
        aux (x : xs) index checkedCoordinates
            | (elem x checkedCoordinates || not (validateCoordinateFormat x)) = index
            | otherwise = aux xs (index + 1) (checkedCoordinates ++ [x])

-- | Returns the index at which the first string of a list is a coordinate or the list's length when a coordinate was not found
listHeadIsCoordinate :: [String] -> Int
listHeadIsCoordinate list = aux list 0
    where
        aux [] index = index
        aux (x : xs) index
            | validateCoordinateFormat x = index
            | otherwise = aux xs (index + 1)
