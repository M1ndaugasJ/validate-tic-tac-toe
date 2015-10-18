module First where

import Data.List
import Data.Char
import Data.List.Split

message :: String
--message = "l[m[x 2; y 2; v \"x\"]; m[x 2; y 1; v \"x\"]; m[x 2; y 0; v \"x\"]; m[x 0; y 0; v \"o\"];m[x 0; y 1; v \"x\"]; m[x 0; y 2; v \"x\"]]"
message = "l[m[x 0; y 0; v \"x\"]; m[x 1; y 1; v \"x\"]; m[x 0; y 2; v \"x\"]; m[x 2; y 2; v \"x\"]; m[x 1; y 2; v \"x\"]]"

type ExternalMap = [InternalMap]
type Coord = Int
data InternalMap = InternalMap { x :: Coord  
                     , y :: Coord  
                     , value :: Char 
                     } deriving (Show)

instance Eq InternalMap where
    (InternalMap i1 x1 _) == (InternalMap i2 x2 _) = (i1 == i2) && (x1 == x2)

instance Ord InternalMap where
    (InternalMap i1 x1 _) <= (InternalMap i2 x2 _) = (i1 <= i2) && (x1 <= x2)

readFullMap :: ExternalMap 
readFullMap = map readInternalMap $ splitIntoEncodedMaps message

readCoordinates :: String -> (String, Coord, Coord)
readCoordinates singleMap =
    let
       (rest, coord) = readTillPairEnd singleMap
       (rest', coord') = readTillPairEnd rest
    in (rest', coord, coord')

readTillPairEnd :: String -> (String, Coord)
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