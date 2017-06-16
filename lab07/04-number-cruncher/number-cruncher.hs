{-# LANGUAGE EmptyDataDecls #-}

import Control.Monad.Reader
import Data.Maybe (fromJust)
import System.Environment
import Data.List
import qualified Data.Map as M

{-
  Написать программу, работа которой управляется конфигурационным файлом,
  содержащим строки следующего формата:
    имя поля=значение
  Возможными именами полей являются summand (слагаемое), multiplier (множитель),
  divisor (делитель). Все значения являются целыми числами. В качестве параметров
  командной строки программе подаются имя конфигурационного файла и имя текстового
  файла с целочисленными данными. Над каждым целым числом из второго файла выполняются
  операции, указанные в конфигурационном файле, то есть число складывается,
  умножается и делится соответственно. Если какое-либо поле отсутствует, то действие
  не выполняется. Результаты вычислений выводятся на консоль.

  Необходимо организовать доступ к параметрам конфигурационного файла средствами
  монады Reader.
-}

data Config = Config Int Int Int deriving (Show)

loadConfig :: FilePath -> IO Config
loadConfig f = (toConfig . map snd . sort . getValues) <$> readFile f
    where toConfig [d, m, s] = Config d m s
          getValues  = M.toList . M.fromList . defaults . map split . lines
          defaults a = [("divisor", 1), ("multiplier", 1), ("summand", 0)] ++ a
          split xs   = toPair (fromJust $ elemIndex '=' xs) xs
          toPair i   = (,) <$> take i <*> (read . drop (i+1))

loadData :: FilePath -> IO [Int]
loadData f = map read . words <$> readFile f

cruncher :: [Int] -> Reader Config [Int]
cruncher xs = do
    Config d m s <- ask
    return $ map (\x -> (x + s) * m `div` d) xs

main :: IO ()
main = do
  [fConf, fData] <- getArgs
  config <- loadConfig fConf
  numbers <- loadData fData
  let results = runReader (cruncher numbers) config :: [Int]
  mapM_ print results
