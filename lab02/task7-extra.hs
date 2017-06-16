import Data.List
import Data.Char
import GHC.Exts

{-
 1) Формирование числовых последовательностей с помощью функции iterate.
  a) Список натуральных чисел, начиная с 0.
  b) Список положительных чётных чисел.
  c) Список элементов последовательности: a_0=1, a_n=(1+a_{n-1})/2.
  d) Список символов английского алфавита.
-}

nats :: [Integer]
nats = iterate (+1) 0

evens :: [Integer]
evens = iterate (+2) 2

series :: [Double]
series = iterate (\x -> (1+x)/2) 1

engAlphabet :: String
engAlphabet = take 26 $ iterate succ 'a'

-- 2) Дан список. Определить длину самого длинного подсписка, содержащего
--     подряд идущие одинаковые элементы (функция group).

longestRepeated :: Eq a => [a] -> Int
longestRepeated = maximum . map length . group

{-
 3) Группировка списков.
  a) Дан список символов. Сгруппировать подряд идущие символы по принципу:
     цифры — не цифры — ...
  b) Дан список и ненулевое натуральное число n. Разбить список на подсписки
     длиной n каждый. Последний подсписок может содержать менее n элементов.
  c) Дан список и ненулевые натуральные числа n и m. Разбить список на
     перекрывающиеся подсписки длиной n элементов со сдвигом относительно
     предыдущего подсписка на m элементов.

  Совет: в решениях могут пригодиться функции group и groupBy, также
  допустимо использование явной рекурсии.
-}

f2a :: String -> [String]
f2a = groupBy (\x y -> isNumber x && isNumber y || isAlpha x && isAlpha y)


f2b :: [a] -> Int -> [[a]]
f2b xs n = f2c xs n n

test_f2b = f2b [1..10] 4 == [[1,2,3,4],[5,6,7,8],[9,10]]


-- TODO: ask how to do the same with groupBy?

f2c :: [a] -> Int -> Int -> [[a]]
f2c xs n m = filter (not.null) division
    where parts    = length xs `div` m
          division = map (take n . flip drop xs . (* m)) [0 .. parts]

test_f2c = f2c [1..10] 4 2 == [[1,2,3,4],[3,4,5,6],[5,6,7,8],[7,8,9,10],[9,10]]

-- 4) Посчитать количество элементов списка, за которыми следуют превосходящие их элементы.

countEls :: [Integer] -> Int
countEls xs = length $ filter (== True) $ zipWith (<) xs (tail xs)

-- 5) Посчитать количество локальных минимумов в целочисленном списке
--  (элемент называется локальным минимумом, если он меньше всех своих соседей).

findLocalMins :: (Ord a) => [a] -> [a]
findLocalMins (x:y:xs) = third $ foldl f (x, y, []) xs
    where
        third (_, _, x) = x
        f (x, y, ls) z = (y, z, if x > y && y < z then y:ls else ls)

countLocMins :: [Integer] -> Int
countLocMins = length . findLocalMins

-- 6) Дан список. Повторить каждый его элемент заданное число раз.
--    Совет: в решении может пригодиться функция concat.

repeat' n a = foldl (\xs _ -> a:xs) [] [1..n]

repeatEls :: Int -> [a] -> [a]
repeatEls = concatMap . replicate

test_repeatEls = repeatEls 2 [1,2,3] == [1,1,2,2,3,3]

-- 7) Найти сумму всех чисел Фибоначчи, удовлетворяющих заданному предикату,
--    в указанном промежутке (например: все чётные от 1 до 106).

condFibsSum :: (Integer -> Bool) -> Integer -> Integer -> Integer
condFibsSum pred from to = sum $ sieve fibs
    where
        sieve = dropWhile (from <=) . takeWhile (to >=) . filter pred
        fibs  = 0 : 1 : zipWith (+) fibs (tail fibs)

test_condFibsSum = condFibsSum even 1 106 == 44

-- 8) Сформировать список строк, представляющих n-значные двоичные числа.

binNumbers :: Int -> [String]
binNumbers 0 = []
binNumbers 1 = ["0", "1"]
binNumbers n = map ('1':) $ genSeq $ n - 1
    where
        genSeq k = take (2^k) $ drop (2^k-2) go
        go = "0" : "1" : concatMap (\a -> [a++"0", a++"1"]) go

test_binNumbers = binNumbers 3 == ["100", "101", "110", "111"]
