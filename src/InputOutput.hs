module InputOutput 
(
readConfiguration,
savePPM,
)
where
import System.IO
import DataStructures
readConfiguration :: String -> IO Configuration
readConfiguration srcPath = do
    hndFile <- openFile srcPath ReadMode
    strConfiguration <- hGetContents hndFile
    let recConfiguration = read strConfiguration :: Configuration
    putStrLn $ "Read : " ++ strConfiguration
    hClose hndFile
    return recConfiguration

colour (Colour r g b) = [channel r, channel g, channel b]

channel :: Double -> Int
channel = floor . (255*) . min 1 . max 0

savePPM :: FilePath -> [[Colour]] -> IO ()
savePPM f img = writeFile f $ buildPPM img

buildPPM :: [[Colour]] -> String
buildPPM img =
  "P3\n" ++ show (length $ head img)
              ++
                " "
                  ++
                    show (length img)
                      ++
                        " 255\n"
                          ++
                            unlines
                              (map unwords $ group 15 $ map show $ concatMap colour $ concat img)

group _ [] = []
group n xs =
  let (xs0,xs1) = splitAt n xs
  in  xs0 : group n xs1


