import First
import Data.Vector
import Data.Set
import Data.List
import Data.Maybe
import Data.Function (on)

--value readFullMap message !! 1
returnCoords :: Int -> Int
returnCoords n = x (readFullMap !! n) + y (readFullMap !! n)

parsedValues = Prelude.map (\n-> value n) $ readFullMap

areAnyCollidedValues = Prelude.length (Data.Set.fromList readFullMap) < Prelude.length readFullMap
areAnySuccessiveMoveValuesEqual = isInfixOf "xx" parsedValues || isInfixOf "oo" parsedValues

returnMapWithValue' :: InternalMap -> Char -> Maybe InternalMap
returnMapWithValue' n valueToCheck = if value n == valueToCheck 
									  then Just n
									  else Nothing

isWinner :: Char -> Bool
isWinner a = 
	let
		valueMap = Prelude.map (\n-> returnMapWithValue' n a) $ readFullMap
		justMap = Prelude.filter (\n-> isJust n) $ valueMap
	in if Prelude.length justMap >= 3 
		then isWinnerMoveLast (checkWinner (Prelude.map (\n-> fromJust n) justMap))
		else False

isWinnerValid :: Bool -> Bool -> Bool
isWinnerValid a b = if a && b then False else a || b

isWinnerMoveLast :: [InternalMap] -> Bool
isWinnerMoveLast a = not . Data.List.null $ Prelude.filter (\n -> n == Data.List.last readFullMap) a

checkWinner :: [InternalMap] -> [InternalMap]
checkWinner internalMaps 
	| Prelude.length (diagonalTopRight) == 3 = diagonalTopRight
	| Prelude.length (diagonalTopLeft) == 3 = diagonalTopLeft
	| Prelude.length (xVertice) == 1 = xVertice !! 0
	| Prelude.length (yVertice) == 1 = yVertice !! 0
	| otherwise = []
	where 
		diagonalTopRight = Prelude.filter (\i1 -> x i1 == y i1) internalMaps 
		diagonalTopLeft = (Prelude.filter (\i1 -> x i1 + y i1 == 2) internalMaps)
		xVertice = Prelude.filter(\n-> (Prelude.length n) == 3) (groupBy (\i1 i2 -> x i1 == x i2) internalMaps)
		yVertice = Prelude.filter(\n-> (Prelude.length n) == 3) (groupBy (\i1 i2 -> y i1 == y i2) internalMaps)

-- checkWinner' :: [InternalMap] -> Bool
-- checkWinner' internalMaps 
-- 	| Prelude.length (Prelude.filter (\i1 -> x i1 == y i1) internalMaps) == 3 = True
-- 	| Prelude.length (Prelude.filter (\i1 -> x i1 + y i1 == 2) internalMaps) == 3 = True
-- 	| Prelude.length (Prelude.filter(\n-> (Prelude.length n) == 3) (groupBy (\i1 i2 -> x i1 == x i2) internalMaps)) == 1 = True
-- 	| Prelude.length (Prelude.filter(\n-> (Prelude.length n) == 3) (groupBy (\i1 i2 -> y i1 == y i2) internalMaps)) == 1 = True
-- 	| otherwise = False

isBoardValid :: Bool
isBoardValid = not (areAnyCollidedValues || areAnySuccessiveMoveValuesEqual) && isWinnerValid (isWinner 'o') (isWinner 'x')
