import Data.List

message :: String
message = "l[m[x 2; y 2; v \"x\"]; m[x 2; y 0; v \"o\"]; m[x 1; y 1; v \"x\"]; m[x 1; y 0; v \"o\"]; m[x 0; y 1; v \"x\"]; m[x 2; y 0; v \"x\"]]"

type Coordinate  = (Char, Char)
type Value  = (Char, Char)
type Coordinates = (Coordinate, Coordinate)
type InternalMap = (Coordinates, String)
type ExternalMap = [InternalMap]

readTillPairEnd :: String -> (String, Coordinate)
readTillPairEnd encodedMap =
    let 
        pair = takeWhileInclusive (/= ';') encodedMap
        coordinate = drop (length pair - 4) pair
        rest = drop (length pair) encodedMap
    in  (rest, (coordinate !! 0, coordinate !! 2))


readInternalMap :: String -> InternalMap
readInternalMap encodedMap = 
	let 
		(rest, coords) = readCoordinates encodedMap


readCoordinates :: String -> (String, Coordinates)
readCoordinates map =
	let
	   (rest, coord) = readTillPairEnd map
	   (rest', coord') = readTillPairEnd rest
	in (rest', (coord, coord'))

takeWhileInclusive :: (a -> Bool) -> [a] -> [a]
takeWhileInclusive _ [] = []
takeWhileInclusive p (x:xs) = x : if p x then takeWhileInclusive p xs
                                         else []