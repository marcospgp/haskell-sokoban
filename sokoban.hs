module Sokoban where

type Coord = (Int,Int)

-- | Validates sokoban input including map and coordinates. Returns OK or the number of the line where an error was found.
validate :: String -- ^ The path to the file containing the data to be parsed
         -> IO ()
validate path = do
    content <- readFile path -- Read the input file
    let fileLines = lines content
    parseLines fileLines 0

-- | Parses the file lines one by one recursively
parseLines :: [String] -- ^ The array of lines to be parsed
           -> Int      -- ^ The number of lines already parsed (to log the line number on error)
           -> Int      -- ^ Returns -1 on success or a line number on error
parseLines lines index
    | null line = -1
    | (head line == '#') =
        if (validateMapLine line) then
            parseLines (tail lines) (index + 1)
        else
            index
    | (isDigit (head line)) =
        if (validateCoordinates line) then
            parseLines (tail lines) (index + 1)
        else
            index
    | otherwise = index
    where
        line = lines !! index

validateMapLine :: [String] -- ^ The array of lines being parsed
                -> Int      -- ^ The number of lines already parsed
                -> Bool     -- ^ True on success, False on error
validateMapLine lines index







-- | Validates a set of coordinates
validateCoordinateLine :: String -- ^ A line of text to be validated as a coordinate
                    -> Bool
