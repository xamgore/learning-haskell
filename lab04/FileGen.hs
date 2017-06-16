module FileGen where

{-
  В этом упражнении необходимо разработать программу --- генератор текстовых файлов.
  Генератор должен поддерживать следующие режимы работы:
   * генерация файла, в котором N раз повторяется заданная строка;
   * генерация файла, содержащего M строк по N случайных целых чисел,
     разделённых пробелами.

  Все необходимые для работы генератора данные (режим работы, имя создаваемого файла,
  число строк и чисел в строке) должны задаваться параметрами командной
  строки. Эти параметры должны сохраняться в специально разработанном типе данных.

  При выполнении задания рекомендуется определять вспомогательные функции.

  Совет: для запуска функции main в ghci с заданием параметров командной строки удобно
  использовать команду интерпретатора :main, например:

     ghci> :main text hello.txt 1000 "Привет, мир"

  или

     ghci> :main random numbers.txt 1000 10
-}

import System.Environment
import System.Random
import Data.List


-- Тип данных для представления параметров генерации
data Params = Text String Int String | Rand String Int Int deriving (Show)

-- Разбор параметров командной строки
parseArgs :: [String] -> Maybe Params
parseArgs [mode, file, count, param]
    | mode == "text"   = Just $ Text file (read count) param
    | mode == "random" = Just $ Rand file (read count) (read param)
parseArgs _ = Nothing

-- Генерация файла
createFile :: Params -> IO ()

createFile (Text file count str) =
    writeFile file $ concat $ replicate count str

createFile (Rand file m n) = do
    gen <- newStdGen
    writeFile file $ join $ rows (randoms gen :: [Integer])
        where
            joinNums = unwords . map show
            join     = intercalate "\n" . map joinNums
            rows  xs = splitBy n $ take (m * n) xs

-- 8 961 3274743 Владимир
-- 8 988 5756151

splitBy :: Int -> [a] -> [[a]]
splitBy n xs = filter (not.null) division
    where parts    = length xs `div` n
          division = map (take n . flip drop xs . (* n)) [0 .. parts]

-- Вывод информации об использовании генератора
usage :: IO ()
usage = do
    putStrLn "text <out.txt> <n> <string>"
    putStrLn "   repeat string n-times and save to file"
    putStrLn "rand <out.txt> <m> <n>"
    putStrLn "   generate file with m-rows of n random nums"

main :: IO ()
main = do
  args <- getArgs
  case parseArgs args of
    Just gp -> createFile gp
    Nothing -> usage
