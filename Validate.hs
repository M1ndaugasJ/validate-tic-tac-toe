import First
import Data.Vector
import Data.Set

--value readFullMap message !! 1
returnCoords :: Int -> String
returnCoords n = [x (readFullMap !! n)] Prelude.++ [y (readFullMap !! n)]

 --x = generate (Prelude.length(readFullMap))(\n-> returnCoords n)

areCollidedValues = Prelude.length (Data.Set.fromList readFullMap) < Prelude.length readFullMap