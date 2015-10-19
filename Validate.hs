import First
import Data.Vector
import Data.Set
import Data.List
import Data.Maybe
import Data.Function (on)

--value readFullMap message !! 1
returnCoords :: Int -> Int
returnCoords n = x (readFullMap !! n) + y (readFullMap !! n)

 --x = generate (Prelude.length(readFullMap))(\n-> returnCoords n)
 --Prelude.filter(\n-> n 'x' returnIsValue) readFullMap

--sort $ Prelude.map (\n-> returnMapWithValue' n 'o') $ readFullMap

parsedValues = Prelude.map (\n-> value n) $ readFullMap

areAnyCollidedValues = Prelude.length (Data.Set.fromList readFullMap) < Prelude.length readFullMap
areAnySuccessiveMoveValuesEqual = isInfixOf "xx" parsedValues || isInfixOf "oo" parsedValues

returnIsValue :: Int -> Char -> Bool
returnIsValue n char = value (readFullMap !! n) == char

returnMapWithValue :: Int -> Maybe InternalMap
returnMapWithValue n = if value (readFullMap !! n) == 'o' 
							then Just(readFullMap !! n)
							else  Nothing

returnMapWithValue' :: InternalMap -> Char -> Maybe InternalMap
returnMapWithValue' n valueToCheck = if value n == valueToCheck 
									 then Just n --Data.List.elemIndex n readFullMap)
									 else Nothing
-- && (Data.List.length parsedValues) <= 4
isBoardValid :: Bool
isBoardValid = (not areAnyCollidedValues || not areAnySuccessiveMoveValuesEqual) && isWinner


isWinner :: Bool
isWinner = 
	let
		xMap = Prelude.map (\n-> returnMapWithValue' n 'x') $ readFullMap
		justXMap = Prelude.filter (\n-> isJust n) $ xMap
		oMap = Prelude.map (\n-> returnMapWithValue' n 'o') $ readFullMap
		justOMap = Prelude.filter (\n-> isJust n) $ oMap
		isOwinner = checkWinner (Prelude.map (\n-> fromJust n) justOMap)
		isXwinner = checkWinner (Prelude.map (\n-> fromJust n) justXMap)
	in if isOwinner && isXwinner 
	   then False 
	   else if isOwinner || isXwinner
	   then True
	   else False

checkWinner :: [InternalMap] -> Bool
checkWinner internalMaps 
				| Prelude.length (Prelude.filter (\i1 -> x i1 == y i1) readFullMap) == 3 = True
				| Prelude.length (Prelude.filter (\i1 -> x i1 + y i1 == 2) readFullMap) == 3 = True
				| Prelude.length (Prelude.filter(\n-> (Prelude.length n) == 3) (groupBy (\i1 i2 -> x i1 == x i2) readFullMap)) == 1 = True
				| Prelude.length (Prelude.filter(\n-> (Prelude.length n) == 3) (groupBy (\i1 i2 -> y i1 == y i2) readFullMap)) == 1 = True
				| otherwise = False
