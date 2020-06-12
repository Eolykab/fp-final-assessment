module DataStructures 
(
Point,
Dimension,
Configuration(..),
Colour(..),
Pixel(..),
Image(..),
GridCell(..),
Location
)
where
type Point = (Double, Double)
type Dimension = (Int, Int)
type Pixel = (Int, Int, Int)
type Location = (Int, Int)
data Configuration =  Configuration { points :: [Point], dimensions :: Dimension, iterations :: Int, imageFile :: String} deriving (Read, Show)
data Colour = Colour {red, green, blue :: Double} deriving (Eq, Show)
data GridCell t = Cell { value :: t
                       , location :: Location
                       } | EmptyCell deriving (Read, Show) 
data Image t = Image { pixels :: [GridCell t]
                     , size :: Dimension
                     } deriving (Read, Show) 