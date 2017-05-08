module IOUtils.CSV(

)where

type Path = String
type DataPoints = ((Double, Double, Double), Integer)

writeCSV :: String -> IO ()
writeCSV filePath = undefined


readCSV :: String -> IO ([DataPoints])
readCSV filePath = undefined
