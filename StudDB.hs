module StudDB where

import System.Environment (getArgs)
import Control.Applicative ()
import Data.Ord (comparing)
import Data.List (sortBy, groupBy)
import Data.Function (on, (&))
import Control.Monad

{-
  Дан текстовый файл с информацией о студентах факультета в следующем формате:

    ФАМИЛИЯ ИМЯ ОТЧЕСТВО;ВОЗРАСТ;КУРС;ГРУППА

  Имя этого файла задаётся параметром командной строки. Остальные параметры определяют,
  какое действие следует выполнить:

  1) Вычислить средний возраст студентов заданной группы заданного курса.
  2) Вычислить количество студентов в каждой группе каждого курса.
  3) Создать файлы (с именами "<КУРС>_<ГРУППА>.txt") со списками всех студентов групп в формате
        ФАМИЛИЯ И.О.

  Старайтесь использовать алгебраические типы данных и вспомогательные функции везде,
  где только возможно.
-}

main :: IO ()
main = do
    args <- getArgs

    case head args of
        "mean"  -> readDB (args!!1) >>= print . meanAge (read $ args!!2) (read $ args!!3)
        "count" -> readDB (args!!1) >>= putStr . unlines . count
        "sort"  -> readDB (args!!1) >>= createFiles
        _  -> do
            putStrLn "  mean  <file> <grade> <group>  -- compute mean age of group's students"
            putStrLn "  count <file>                  -- count students in each group and grade"
            putStrLn "  sort  <file>                  -- create sort of files with short names"



data Student = Student { name :: String, age :: Int, grade :: Int, group :: Int } deriving (Show)



meanAge :: Int -> Int -> [Student] -> Double
meanAge gd gp = mean . map age . filter (\s -> grade s == gd && group s == gp)
    where mean xs = realToFrac (sum xs) / realToFrac (length xs)


createFiles :: [Student] -> IO ()
createFiles = mapM_ write . changeFileNames . getStudentNames
    where write = uncurry $ flip writeFile

changeFileNames :: [(String, Group)] -> [(String, FilePath)]
changeFileNames = map (fmap toFileName)
    where toFileName (gd, gr) = show gd ++ "_" ++ show gr ++ ".txt"

getStudentNames :: [Student] -> [(String, Group)]
getStudentNames = ((unlines . map formatName) & ofGroups) . sieve

formatName :: Student -> String
formatName s = first ++ " " ++ [head second] ++ "." ++ [head last'] ++ "."
    where [first, second, last'] = words $ name s



-- describe count info
describe :: (Int, Group) -> String
describe (cnt, (gd, gr)) = "Group " ++ show gd ++ show gr ++ " = " ++ show cnt

count :: [Student] -> [String]
count = map describe . (length & ofGroups) . sieve



-- student's (Grade, Group)
type Group = (Int, Int)

ofGroups :: ([Student] -> a) -> [[Student]] -> [(a, Group)]
ofGroups f xs = zip (map f xs) (info xs)

-- set of common representatives
info :: [[Student]] -> [Group]
info = map (pair . head)

sieve :: [Student] -> [[Student]]
sieve = groupBy ((==) `on` pair) . sortBy (comparing pair)

pair :: Student -> Group
pair = liftM2 (,) grade group



readDB :: FilePath -> IO [Student]
readDB f = (map parse . lines) <$> readFile f

parse :: String -> Student
parse str = Student name' (read age') (read grade') (read group')
    where [name', age', grade', group'] = splitOn ';' str

splitOn :: Char -> String -> [String]
splitOn ch s =
    case dropWhile (== ch) s of
        "" -> []
        s' -> w : splitOn ch s''
              where (w, s'') = break (== ch) s'
