import Control.Arrow

{-
  Все задачи в этом упражнении должны решаться исключительно с помощью свёрток.
  Явная рекурсия не допускается. Если в решении в качестве вспомогательной
  требуется стандартная функция обработки списков (помимо fold*, scan*), она
  также должна реализовываться свёрткой.

  Каждое решение должно сопровождаться как минимум тремя различными тестовыми
  примерами, например:

  f :: тип
  f = решение

  test_f = f undefined == undefined -- тест 1
           && f undefined == undefined -- тест 2
           && f undefined == undefined -- тест 3
-}

{-
 1. Простейшие функции обработки списков
  a) Найти сумму чётных элементов списка с целочисленными элементами.
  b) Найти среднее арифметическое элементов списка вещественных чисел (функцией length
     пользоваться нельзя, решение должно выполняться в один проход).
  c) Найти минимальный элемент списка.
  d) Найти наименьший нечётный элемент списка с целочисленными значениями (дополнительным
     параметром функции должно быть значение, возвращаемое по умолчанию).
-}

sumEvens :: (Integral a) => [a] -> a
sumEvens = foldl addEven 0
    where addEven x y = x + (if even y then y else 0)

test_sumEvens = and $
    map (\x -> sumEvens [1..x] == (sum $ filter even [1..x])) [1..1000]


mean :: (Fractional a) => [a] -> a
mean = uncurry (/) . foldr (\elem (sum, count) -> (elem + sum, count + 1)) (0, 0)

test_mean = and $ [ mean [1..1] == 1, mean [1..3] == 2, mean [1..10] == 5.5 ]


minVal :: (Integral a, Ord a) => [a] -> a
minVal = foldl1 min

test_minVal = and $ [ minVal [1..10] == 1, minVal [-1] == -1, minVal [2, 4, 6] == 2 ]


minOddVal :: (Integral a, Ord a) => a -> [a] -> a
minOddVal def = snd . foldl f (False, def)
    where
        f (exists, val) el
            -- skip even elements
            | even el    = (exists, val)
            -- el is odd, we've found first min
            | not exists = (True, el)
            -- el is odd, improve min value
            | otherwise  = (True, min val el)

test_minOddVal = and [
        minOddVal 5 [2, 2, 2, 4, 8, 10, 2, 0, 21, 1, 3, -1, 5, 7, 0] == -1,
        minOddVal 5 [2, 2, 2, 2, 2] == 5,
        minOddVal 4 [] == 4
    ]


{-
 2. Свёртки, формирующие списки
  a) Сформировать список, содержащий первые n элементов исходного.
  b) Сформировать список, содержащий последние n элементов исходного.
  c) Сформировать список, содержащий все элементы исходного списка, большие левого соседа.
  d) Сформировать список, содержащий все локальные минимумы исходного списка.
  e) Повторить каждый элемент списка заданное количество раз.
  f) Удалить из списка повторяющиеся подряд идущие элементы.
  g) Даны два списка одинаковой длины. Сформировать список, состоящий из результатов применения
     заданной функции двух аргументов к соответствующим элементам исходных списков.
-}

takeFirst n = fst . foldl f ([], 0)
    where
        f (xs, len) _ | len >= n = (xs, len)
        f (xs, len) x = (xs ++ [x], len+1)

test_takeFirst = and [
        takeFirst 5 [1..10] == [1..5],
        takeFirst 5 [1..3] == [1..3],
        takeFirst 0 [1..10] == []
    ]


takeLast n = fst . foldr f ([], 0)
    where
        f _ (xs, len) | len >= n = (xs, len)
        f x (xs, len) = (x:xs, len+1)

test_takeLast = and [
        takeLast 5 [1..10] == [6..10],
        takeLast 5 [1..3] == [1..3],
        takeLast 0 [1..10] == []
    ]


greaterThanLeft list@(a:_) =
    snd $ foldl f (a, []) list
        where
            f (left, xs) x | left < x  = (x, xs ++ [x])
            f (_, xs)    x             = (x, xs)

test_greaterThenLeft = and [
        greaterThanLeft [1..10] == [2..10],
        greaterThanLeft [1, 2, 1, 2] == [2, 2],
        null $ greaterThanLeft [10, 9..1]
    ]


findLocalMins :: (Ord a) => [a] -> [a]
findLocalMins (x:y:xs) = third $ foldl f (x, y, []) xs
    where
        third (_, _, x) = x
        f (x, y, ls) z = (y, z, if x > y && y < z then y:ls else ls)

test_findLocalMins = and [
        findLocalMins [1, 0, 1] == [0],
        null $ findLocalMins [1, 0, 0, 1],
        findLocalMins [1, 0, 1, 0, 1] == [0, 0]
    ]


repeat' a n = foldl (\xs _ -> a:xs) [] [1..n]

enlarge n = foldr (\x xs -> repeat' x n ++ xs) []

test_enlarge = and [
        enlarge 1 [1, 2, 3,4] == [1,2,3,4],
        enlarge 2 [1, 2] == [1,1,2,2],
        null $ enlarge 0 [1,2,3,4]
    ]


removeDupls = foldr f []
    where
        f x [] = [x]
        f x s@(y:_)
            | x == y    = s
            | otherwise = x:s

test_removeDupls = and [
        removeDupls [1,1,2,2,2,1,1,1,0,1,1] == [1,2,1,0,1],
        removeDupls [1..10] == [1..10],
        removeDupls [1,1,1,1] == [1],
        removeDupls [1] == [1],
        null $ removeDupls []
    ]


zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f = foldr step done
    where
        done ys              = []
        step x zipsfn []     = []
        step x zipsfn (y:ys) = f x y : zipsfn ys

test_zipWith' = and [
        zipWith' (,) [1, 2] [1, 2] == [(1,1), (2,2)],
        zipWith' (,) [1] [1] == [(1,1)],
        zipWith' (,) [1, 2] [1] == [(1,1)],
        zipWith' (,) [1] [1, 2] == [(1,1)]
    ]

{-
 3. Использование свёртки как носителя рекурсии (для запуска свёртки можно использовать
   список типа  [1..n]).
  a) Найти сумму чисел от a до b.
  b) Найти сумму факториалов чисел от a до b (повторные вычисления факториалов не допускаются).
  c) Пользуясь рядом Тейлора, вычислить значение синуса заданного числа x (использовать
     n слагаемых).
-}

sum' a b = foldl1 (+) [a..b]

test_sum' = and [
        sum' 0 0 == 0,
        sum' 1 10 == 55,
        sum' (-10) (-1) == -55
    ]


sumFactorials :: (Integral a) => a -> a -> a
sumFactorials a b = foldl1 (+) facts
    where
        first = foldl1 (*) [1..a]
        facts = scanl  (*) first [a+1..b]

test_sumFactorials = and [
        sumFactorials 4 4 == 24,
        sumFactorials 4 5 == 24 + 120,
        sumFactorials 1 5 == sum [1, 2, 2*3, 2*3*4, 2*3*4*5]
    ]


calcSin x n = foldl1 (+) series
    where
        series = scanl f x [3,5..(2*n+1)]
        f m s = (-1) * m * x * x / s / (s-1)

equals v = abs(sin v - calcSin v 1000) < 0.0001

test_calcSin = and [
        equals $ pi/2,
        equals $ pi/6,
        equals $ pi/3,
        equals 0
    ]

{-
 4. Решить задачу о поиске пути с максимальной суммой в треугольнике (см. лекцию 2) при условии,
   что необходимо дополнительно найти сам путь (к примеру, в виде закодированных направлений спуска:
   0 - влево, 1 - вправо). В решении допускается использование любых стандартных функций.
-}

data Direction = L | R deriving (Eq, Ord, Show)

downstep row = zipWith add theBest
    where
        add (x, y) z = (x + z, y)
        theBest = zipWith max toLeft toRight
        toLeft  = step R ((0, []) : row)
        toRight = step L (row ++ [(0, [])])
        step d  = map (second ((:) d))

answer = rev . maximum . foldl downstep []
    where rev (v, h) = (v, tail $ reverse h)

test = answer [[3],[7,4],[2,4,6],[8,5,9,3]] == (23, [L,R,R])
