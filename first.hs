module First where

import Data.List
import Data.Char
import Data.List.Split

message :: String
message = "l[m[x 2; y 2; v \"x\"]; m[x 2; y 0; v \"o\"]; m[x 1; y 1; v \"x\"]; m[x 1; y 0; v \"o\"]; m[x 0; y 1; v \"x\"]; m[x 2; y 0; v \"x\"]]"

type ExternalMap = [InternalMap]

data InternalMap = InternalMap { x :: Int  
                     , y :: Int  
                     , value :: Char 
                     } deriving (Show)

readFullMap = map readInternalMap $ splitIntoEncodedMaps message

readCoordinates :: String -> (String, Int, Int)
readCoordinates singleMap =
    let
       (rest, coord) = readTillPairEnd singleMap
       (rest', coord') = readTillPairEnd rest
    in (rest', coord, coord')

readTillPairEnd :: String -> (String, Int)
readTillPairEnd encodedMap = 
    let
        string = dropWhile (reqNum) encodedMap
        coord = digitToInt $ head string
    in (drop 1 string, coord)

readValue :: String -> Char
readValue restInternalMap = head $ dropWhile (xORo) restInternalMap

readInternalMap :: String -> InternalMap
readInternalMap encodedMap =
    let 
        (rest, x, y) = readCoordinates encodedMap
        value = readValue rest
    in InternalMap x y value

splitIntoEncodedMaps :: String -> [String]
splitIntoEncodedMaps "" = error "empty string passed"
splitIntoEncodedMaps s = splitOn "m" (drop 3 (filter (/=' ') (s)))

xORo :: Char -> Bool
xORo 'x' = False
xORo 'o' = False
xORo _ = True

reqNum :: Char -> Bool
reqNum '0' = False
reqNum '1' = False
reqNum '2' = False
reqNum _ = True