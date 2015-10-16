import First
import Data.Vector
import Data.Set
import Data.List

--value readFullMap message !! 1
returnCoords :: Int -> Int
returnCoords n = x (readFullMap !! n) + y (readFullMap !! n)

 --x = generate (Prelude.length(readFullMap))(\n-> returnCoords n)
 --Prelude.filter(\n-> n 'x' returnIsValue) readFullMap

--(Prelude.map (\n-> value n) $ readFullMap)

parsedValues = (Prelude.map (\n-> value n) $ readFullMap)

areCollidedValues = Prelude.length (Data.Set.fromList readFullMap) < Prelude.length readFullMap
areAnySuccessiveMovesEqual = isInfixOf "xx" parsedValues || isInfixOf "oo" parsedValues

returnIsValue :: Int -> Char -> Bool
returnIsValue n char = value (readFullMap !! n) == char
