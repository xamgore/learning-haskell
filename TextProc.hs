module TextProc where

import System.Environment
import Control.Applicative ()
import Control.Monad
import Data.Char (toUpper)
import System.Random (randomRIO)

{-
  Разработайте утилиту со следующими возможностями:
  1) подсчёт количества строк в заданном текстовом файле;
  2) добавление заданной строки в начало (конец) заданного файла;
  3) преобразование всех буквенных символов заданного файла к верхнему
     регистру (результат выводится на консоль);
  4) построчное слияние двух заданных файлов (каждая строка первого файла
     соединяется с соответствующей строкой второго файла);
  5) генерация случайного текстового файла (случайность должна ограничиваться
     максимальным количеством строк в файле и символов в строке).

  Все входные данные программы должны задаваться с помощью параметров
  командной строки. Старайтесь использовать алгебраические типы данных
  и вспомогательные функции везде, где только возможно.
-}


main :: IO ()
main = do
    args <- getArgs

    unless (null args) $
        case head args of
            "upper" -> upper (args!!1)
            "count" -> count (args!!1)
            "merge" -> merge (args!!1) (args!!2)
            "gen"   -> gen   (args!!1) (read $ args!!2)
            "add"   -> add   (args!!2) (args!!3) (args!!1)
            _ -> do
                putStrLn "    count <file>            подсчёт количества строк в файле"
                putStrLn "    upper <file>            преобразование символов файла к верхнему регистру"
                putStrLn "    gen   <file>  <lines>   генерация случайного файла"
                putStrLn "    merge <file1> <file2>   построчное слияние двух заданных файлов"
                putStrLn "    add head <file> <line>  добавление заданной строки в начало файла"
                putStrLn "    add tail <file> <line>                             в конец  файла"


count :: FilePath -> IO ()
count f = readFile f >>= print . length . lines

upper :: FilePath -> IO ()
upper f = readFile f >>= putStrLn . map toUpper

add :: FilePath -> String -> String -> IO ()
add f s p = readFile f >>= writeFile f . unlines . insert . lines
    where insert ls = if p == "head" then s : ls else ls ++ [s]

merge :: FilePath -> FilePath -> IO ()
merge f1 f2 = putStrLn =<< unlines <$> zipLines (linesOf f1) (linesOf f2)
    where zipLines = liftM2 (zipWith (\x y -> x ++ "\n" ++ y))
          linesOf  = fmap lines . readFile

gen :: FilePath -> Int -> IO ()
gen f n = writeFile f =<< fmap unlines rText
    where rText = replicateM n $ replicateM 80 rChar
          rChar = (alpha !!) <$> randomRIO (0, length alpha - 1)
          alpha = ['a'..'z'] ++ ['A'..'Z'] ++ replicate 10 ' '
