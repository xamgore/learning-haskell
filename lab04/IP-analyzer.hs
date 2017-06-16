{-# LANGUAGE EmptyDataDecls #-}

import System.Environment
import Data.Function
import Data.List
import Data.Ord

{-
  Разработайте тип данных Host для хранения следующей информации:
   * IP-адрес (строка)
   * Имя хоста
   * Страна
-}

data Host = Host { ip :: String, name :: String, country :: String } deriving Show

{-
   Реализуйте функцию, которая по заданной строке возвращает значение типа Host.
   Формат строки: IP-АДРЕС ИМЯ СТРАНА
   Название страны может состоять из более чем одного слова.
-}

parseHost :: String -> Host
parseHost = f . words
    where
        f (i:n:ws) = Host { ip = i, name = n, country = unwords ws }
        f other    = error $ "smth strange with input " ++ unwords other

{-
   Реализуйте функцию, которая загружает из файла с заданным именем
   список пользователей. Для проверки используйте файл hosts.txt.
-}

loadData :: FilePath -> IO [Host]
loadData fname = do
    content <- readFile fname
    return $ map parseHost $ lines content

{-
   Напишите функции, которые по заданному списку хостов находят следующую
   информацию:
   * Отчёт 1: количество хостов из страны с заданным названием
   * Отчёт 2: количество различных стран (может оказаться полезной функция nub)
   * Отчёт 3: список хостов с именами максимальной длины
   * Отчёт 4: страна с наибольшим количеством хостов

   Явная рекурсия в отчётах не допускается, следует использовать функции
   высших порядков.
-}

report1 :: String -> [Host] -> Int
report1 c = length . filter (== c) . map country

-- report1 "Russia" == 56

report2 :: [Host] -> Int
report2 = length . nubBy ((==) `on` country)

-- report2 == 250

report3 :: [Host] -> [Host]
report3 hosts = filter ((== m) . length . name) hosts
    where   m = maximum $ map (length . name) hosts

-- report3 == navshipyd-pearl-harbor

report4 :: [Host] -> [String]
report4 hosts = map (country . head) maxgrs
    where
        maxlen = length (head clases)
        maxgrs = takeWhile ((== maxlen) . length) clases
        clases = sortBy (flip $ comparing length) groups
        groups = groupBy ((==) `on` country) sorted
        sorted = sortBy (comparing country) hosts

-- report4 == ["Belize","Dominican Republic"]

{-
   Напишите основную программу, которая читает параметр командной строки --- имя файла,
   загружает из него список хостов и печатает отчёты 1-4.
-}

main :: IO ()
main = do
  args <- getArgs
  hosts <- loadData $ head args

  putStr "\nAmount of hosts from Russia: "
  print $ report1 "Russia" hosts

  putStr "Amount of countries: "
  print $ report2 hosts

  putStrLn "\nList of hosts with max length:"
  print $ report3 hosts

  putStrLn "\nCountries with maximum hosts:"
  print $ report4 hosts
