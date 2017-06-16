module Coordinates where

import System.Environment
import System.Random
import Data.List
import Data.Ord

{-
  Написать программу, которая в зависимости от параметров командной строки
  а) генерирует случайный текстовый файл содержащий декартовы координаты точек на плоскости
     (по одной точке в каждой строке);
  б) определяет по заданному файлу в указанном ранее формате количество точек в каждой
     из четвертей;
  в) отыскивает наиболее удалённую от начала координат точку.
-}

main :: IO ()
main = do
    args <- getArgs

    case head args of
        "search"   -> search (args!!1)
        "analyze"  -> analyze (args!!1)
        "generate" -> generate (args!!1) (read $ args!!2)
        _ -> do
            putStrLn "generate <file> <count> -- generate random points"
            putStrLn "analyze  <file>         -- count points in every quoter"
            putStrLn "search   <file>         -- the farthest point from the center"


parsePoints :: String -> [(Int, Int)]
parsePoints = slide . map read . words
    where slide (x:y:ys) = (x, y) : slide ys
          slide _        = []


search :: String -> IO ()
search file = readFile file >>= print . maxDist . parsePoints
    where maxDist = maximumBy (comparing (\(x, y) -> x*x + y*y))


generate :: String -> Int -> IO ()
generate file count =
    mapM sequence [[r, r] | _ <- [1..count]]
        >>= (writeFile file . join)
    where
        r    = randomRIO (-100, 100) :: IO Int
        join = intercalate "\n" . map (unwords . map show)


analyze :: String -> IO ()
analyze file = readFile file >>= describe . countQuoters . parsePoints
    where
        boolToInt x  = if x then 1 else 0
        countQuoters = foldl f [0, 0, 0, 0]
        f cs (x, y)  = zipWith (+) cs $ map boolToInt [
                x >= 0 && y >= 0, x >= 0 && y <= 0,
                x <= 0 && y <= 0, x <= 0 && y >= 0
            ]

        describe [ur, dr, dl, ul] = do
            putStrLn $ "Upper: " ++ show ul ++ " | " ++ show ur
            putStrLn $ "Lower: " ++ show dl ++ " | " ++ show dr
