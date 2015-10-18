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

returnMapWithValue' :: InternalMap -> Char -> Maybe (InternalMap, Maybe Int)
returnMapWithValue' n valueToCheck = if value n == valueToCheck 
									 then Just (n, Data.List.elemIndex n readFullMap)
									 else Nothing

isBoardValid :: Bool
isBoardValid = if not (areAnyCollidedValues || areAnySuccessiveMoveValuesEqual) && (Data.List.length parsedValues) <= 4
 				then True else isWinner

isWinner :: Bool
isWinner = 
	let
		xMap = Prelude.map (\n-> returnMapWithValue' n 'x') $ readFullMap
		justXMap = Prelude.filter (\n-> isJust n) $ xMap
		oMap = Prelude.map (\n-> returnMapWithValue' n 'o') $ readFullMap
		justOMap = Prelude.filter (\n-> isJust n) $ oMap
		bool = if Prelude.length justXMap >= 3 || Prelude.length justOMap >= 3 then True else False
	in bool

--myGroup' :: (Eq a, Ord a) => [a] -> [a]
--myGroup' = Prelude.map (\l -> l) . groupBy ((==) `on` a)

--Prelude.filter(\n-> (length n) == 3) (groupBy (\i1 i2 -> x i1 == x i2) readFullMap) jei yra tai laimetojas stacias
--Prelude.filter(\n-> (length n) == 3) (groupBy (\i1 i2 -> y i1 == y i2) readFullMap) jei yra laimetojas gulscias
--Prelude.filter (\i1 -> x i1 == y i1) readFullMap is desines i kaire
--Prelude.filter (\i1 -> x i1 + y i1 == 2) readFullMap readFullMap)

--checkWinner :: [Maybe (InternalMap, Maybe Int)] -> Bool
--checkWinner


--patikrint ar nera susidurimu
--patikrint ar nera vienodu values is eiles ejimuose
--tada paziuret ar yra x ar o daugiau nei trys, jei yra, paimt ir patikrint
--paimi visus su o ar x, isrikiuoti, jei randi tris lygias x tada staciai laimejo
--jei tris lygius y tada laimejo gulsciai, 
--istrizai is desines i kaire jei randi tris lygias
--koord 22 11 00, is kaires i desine 02 11 20