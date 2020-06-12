module Process 
(

)
where
import DataStructures
import Data.List

sortDist :: Ord a1 => (a1, (a2, b1)) -> (a1, (a2, b1)) -> Ordering 
sortDist (d1, (_, _)) (d2, (_,_))
    | d1 > d2 = GT
    | d1 < d2 = LT
    | d1 == d2 = EQ
 
 
findNeighbours :: Int -> [Point] -> Point -> [(Double, Point)]
findNeighbours _ [] (_, _) = []
findNeighbours n points pt = take n sorted
    where notSelf = filter (/= pt) points
          dists = calcDistances pt notSelf
          sorted = sortBy sortDist dists


calcDistances :: Point -> [Point] -> [(Double, Point)]
calcDistances (_, _) [] = []
calcDistances pt ps = zip ds ps
    where ds = map calcDist ps
          calcDist =  distance pt

distance :: Point -> Point -> Double
distance (x1, y1) (x2, y2) = 
    let
        deltaXs2 = (x1 - x2) ** 2
        deltaYs2 = (y1 - y2) ** 2
        sumOfSquares = deltaXs2 + deltaYs2
    in sqrt sumOfSquares

buildHeatMap :: Configuration -> Image
buildHeatMap c = img
      where pts = points c
            ds = dimensions c
            i = iterations c
            iFile = imageFile c
            nearest = findNeighbours i pts
            img = Image {pixels = nearest, size= ds}


instance Functor GridCell where
    fmap f cell = Cell {value = f v, location = l}
        where v = value cell
              l = location cell 

instance Functor Image where
        fmap f img = Image {pixels = content, size = s}
            where content = [Cell {value = f (value p), location = location p} | p <- ps ]
                  ps = pixels img
                  s = size img