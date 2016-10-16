module Main where

{-
   Основная программа должна принимать два параметра командной строки — имена
   входного файла со списком точек и выходного файла со списком точек, образующих
   выпуклую оболочку. Формат файлов произвольный.
-}

import GrahamScan
import System.Environment

main = do
    args <- getArgs

    readFile (head args) >>=
        writeFile (last args) . toString . grahamScan . parsePoints


parsePoints :: String -> [Point]
parsePoints = slide . map read . words
    where slide (x:y:ys) = Point x y : slide ys
          slide _        = []


toString :: [Point] -> String
toString = unlines . map (\ pt -> unwords [show $ x pt, show $ y pt])
