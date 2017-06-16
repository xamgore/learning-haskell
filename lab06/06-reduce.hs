import System.Environment
import Data.Maybe (listToMaybe)
import System.Random (randomRIO)

{-
  Напишите функцию reduce, принимающую один целочисленный аргумент a и возвращающую 0,
  если аргумент делится на 3, a^2, если он на 3 не делится и является при этом нечётным,
  a^3 в остальных случаях.
-}

reduce :: Integral a => a -> a
reduce a | a `mod` 3 == 0 = 0
reduce a | odd a          = a*a
reduce a = a*a*a

{-
  Напишите функцию, применяющую функцию reduce заданное количество раз к значению в контексте,
  являющемся функтором:
-}

reduceNF :: (Functor f, Integral a) => Int -> f a -> f a
reduceNF n = fmap (applyNTimes reduce)
    where applyNTimes f = last $ take (n+1) $ iterate (fmap f) id

{-
  Реализуйте следующие функции-преобразователи произвольным, но, желательно, осмысленным и
  нетривиальным способом.
-}

toList :: Integral a => [(a, a)]  -> [a]
toList = map (uncurry (+))

toMaybe :: Integral a => [(a, a)]  -> Maybe a
toMaybe = listToMaybe . toList

toEither :: Integral a => [(a, a)]  -> Either String a
toEither = cast . toList
    where cast []    = Left "empty!"
          cast (a:_) = Right a

-- воспользуйтесь в этой функции случайными числами
toIO :: Integral a => [(a, a)]  -> IO a
toIO = getAny . toList
    where getAny arr = (arr !!) <$> randomRIO (0, length arr - 1)

{-
  В параметрах командной строки задано имя текстового файла, в каждой строке
  которого записана пара целых чисел, разделённых пробелами. Загрузите
  данные из файла в список пар целых чисел, преобразуйте этот список к
  значениям в контекстах [], Maybe, Either String и IO и примените к каждому
  из контекстов функцию reduceNF (значение N также должно браться из
  параметров командной строки).
-}

parseArgs :: [String] -> (FilePath, Int)
parseArgs [path, n] = (path, read n)

readData :: FilePath -> IO [(Int, Int)]
readData f = slide . map read . words <$> readFile f
    where slide (x:y:ys) = (x, y) : slide ys
          slide _        = []

main = do
  (fname, n) <- parseArgs `fmap` getArgs
  ps <- readData fname
  print $ reduceNF n (toList ps)
  print $ reduceNF n (toMaybe ps)
  print $ reduceNF n (toEither ps)
  reduceNF n (toIO ps) >>= print

{-
  Подготовьте несколько тестовых файлов, демонстрирующих особенности различных контекстов.
  Скопируйте сюда результаты вызова программы на этих файлах.
-}
