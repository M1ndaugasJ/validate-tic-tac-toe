module First where

import Data.List
import Data.Char
import Data.List.Split

message :: String
message = "l[m[\"x\"; 2; \"y\"; 0; \"v\"; \"x\"]; m[\"x\"; 1; \"y\"; 1; \"v\"; \"o\"]; m[\"x\"; 2; \"y\"; 1; \"v\"; \"x\"]; m[\"x\"; 1; \"y\"; 0; \"v\"; \"o\"]; m[\"x\"; 2; \"y\"; 2; \"v\"; \"x\"]; m[\"x\"; 0; \"y\"; 1; \"v\"; \"o\"]; m[\"x\"; 0; \"y\"; 0; \"v\"; \"x\"]; m[\"x\"; 1; \"y\"; 2; \"v\"; \"o\"]; m[\"x\"; 0; \"y\"; 2; \"v\"; \"x\"]]";
--message = "l[m[\"x\"; 1; \"y\"; 0; \"v\"; \"x\"]; m[\"x\"; 0; \"y\"; 1; \"v\"; \"o\"]; m[\"x\"; 1; \"y\"; 2; \"v\"; \"x\"]; m[\"x\"; 2; \"y\"; 2; \"v\"; \"o\"]; m[\"x\"; 0; \"y\"; 0; \"v\"; \"x\"]; m[\"x\"; 2; \"y\"; 0; \"v\"; \"o\"]; m[\"x\"; 0; \"y\"; 2; \"v\"; \"x\"]; m[\"x\"; 2; \"y\"; 1; \"v\"; \"o\"]; m[\"x\"; 2; \"y\"; 2; \"v\"; \"x\"]]"
type ExternalMap = [InternalMap]
type Coord = Int
data InternalMap = InternalMap  {x :: Coord  
                     , y :: Coord  
                     , value :: Char 
                     } deriving (Show)

instance Eq InternalMap where
    (InternalMap x1 y1 _) == (InternalMap x2 y2 _) = (x1 == x2) && (y1 == y2)

instance Ord InternalMap where
    compare (InternalMap x y _) (InternalMap x1 y1 _)
         | (x == x1) && (y == y1) =  EQ
         | x <= x1 && y <= y1 =  LT
         | otherwise =  GT

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
        string = dropWhile (\n -> n /= '0' && n /= '1' && n /= '2') encodedMap
        coord = digitToInt $ head string
    in (drop 1 string, coord)

readValue :: String -> Char
readValue restInternalMap = head $ dropWhile (\n -> n /= 'x' && n /= 'o') restInternalMap

readInternalMap :: String -> InternalMap
readInternalMap [] = error "map is empty"
readInternalMap encodedMap =
    let 
        (rest, x, y) = readCoordinates encodedMap
        value = readValue rest
    in InternalMap x y value

splitIntoEncodedMaps :: String -> [String]
splitIntoEncodedMaps [] = error "string is empty"
splitIntoEncodedMaps s = splitOn "m" (drop 3 (filter (/=' ') (s)))