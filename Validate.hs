import First
import Data.Vector
import Data.Set
import Data.List
import Data.Maybe

returnCoords :: Int -> Int
returnCoords n = x (readFullMap !! n) + y (readFullMap !! n)

parsedValues = Prelude.map (\n-> value n) $ readFullMap

areAnyCollidedValues = Prelude.length (nub readFullMap) < Prelude.length readFullMap
areAnySuccessiveMoveValuesEqual = isInfixOf "xx" parsedValues || isInfixOf "oo" parsedValues

valueMapX = Prelude.filter (\n-> value n == 'x') $ readFullMap
valueMapO = Prelude.filter (\n-> value n == 'o') $ readFullMap

winnerMapX = checkWinner valueMapX
winnerMapO = checkWinner valueMapO

isWinnerValid = if noWinner
	then True
	else if not areBothWinners 
	then isWinnerMoveLast getWinner
	else False

getWinner = if Data.List.null (winnerMapX) then winnerMapO else winnerMapX 
areBothWinners = (not . Data.List.null $ winnerMapX) && (not . Data.List.null $ winnerMapO)
noWinner = Data.List.null (winnerMapX) && Data.List.null (winnerMapO)

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
		xVertice = Prelude.filter(\n-> (Prelude.length n) == 3) (groupBy (\i1 i2 -> x i1 == x i2) (sortBy sortByX internalMaps))
		yVertice = Prelude.filter(\n-> (Prelude.length n) == 3) (groupBy (\i1 i2 -> y i1 == y i2) (sortBy sortByY internalMaps))

sortByX (InternalMap x y _) (InternalMap x1 y1 _) = compare x x1
sortByY (InternalMap x y _) (InternalMap x1 y1 _) = compare y y1

isBoardValid :: Bool
isBoardValid = if Prelude.length (nub readFullMap) >= 5 
	then not areAnyCollidedValues && not areAnySuccessiveMoveValuesEqual && isWinnerValid
	else not areAnyCollidedValues && not areAnySuccessiveMoveValuesEqual