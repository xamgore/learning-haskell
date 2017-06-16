{-
  В параметрах командной строки указаны имена нескольких текстовых файлов,
  содержащих целые числа, разделённые пробелами и символами перевода строк.
  Пользуясь множествами, определить:
  A) какие и сколько чисел содержатся в каждом из данных файлов (пересечение);
  B) какие и сколько чисел содержатся хотя бы в одном из данных файлов (объединение).
-}

import qualified Data.IntSet as S
import System.Environment
import Control.Applicative ()
import Control.Monad


readNumFile :: FilePath -> IO [Int]
readNumFile f = (map read . words) <$> readFile f

readAllFiles :: [FilePath] -> IO [[Int]]
readAllFiles = mapM readNumFile


info :: S.IntSet -> (Int, [Int])
info = liftM2 (,) S.size S.toList

solveA, solveB :: [[Int]] -> (Int, [Int])
solveA = info . foldl1 S.intersection . map S.fromList
solveB = info . S.unions . map S.fromList


main :: IO ()
main = do
    xs <- getArgs >>= readAllFiles

    print $ solveA xs
    print $ solveB xs
