{-
  В этом упражнении рассматриваются различные способы объявления
  функций:
    * однострочное определение
    * сопоставление с образцом (pattern matching)
    * условное определение (охранные выражения, guarded expressions)
  и их комбинации.
-}

-- 1) Удвоение значения заданного числа
-- (типовая аннотация здесь означает, что функция принимает один параметр
-- типа a и возвращает значение типа a, причём тип a принадлежит классу типов Num)
double :: Num a => a -> a
double a = a * a

-- 2) Функция, возвращающая True тогда и только тогда, когда оба ее аргумента равны True.
-- После типовой аннотации в объявлении функции следуют два варианта ("clause") её
-- определения, первый вариант соответствует ситуации, когда оба параметра равны True,
-- второй вариант используется во всех остальных случаях.
-- Слева от знака '=' и после имени функции стоят образцы параметров, они могут быть
-- именами переменных, константами или особого вида выражениями

bothTrue :: Bool -> Bool -> Bool
bothTrue True True = True
bothTrue _  _ = False

-- 3) Функция, возвращающая True, если только один из её аргументов равен True,
-- и False в противном случае (пользоваться стандартными логическими операциями не следует,
-- определение может содержать несколько вариантов)

oneTrue :: Bool -> Bool -> Bool
oneTrue True False = True
oneTrue False True = True
oneTrue _ _        = False

-- 4) Определение знака числа (-1, 0, 1)
-- В следующей типовой аннотации тип a должен принадлежать одновременно
-- классам Num и Ord, то есть его значения можно использовать в
-- арифметических операциях (Num) и сравнивать между собой (Ord).
-- После знака '|' указывается "охранное выражение", результатом
-- функции оказывается выражение, следующее за первым истинным охранным
-- выражением (otherwise является синонимом значения True, это всегда
-- истинное охранное выражение).

sign :: (Num a, Ord a) => a -> Int
sign a
   | a < 0 = -1 -- если a < 0
   | a == 0 = 0 -- иначе, если a равно 0
   | otherwise = 1 -- во всех остальных случаях

-- Сопоставление с образцом можно совмещать с охранными выражениями, например:

sign' :: (Num a, Ord a) => a -> Int
sign' 0 = 0
sign' a -- параметр гарантированно не равен 0
   | a < 0 = -1
   | otherwise = 1

-- 5) Утроение заданного числа
-- (типовую аннотацию и образцы параметров следует написать самостоятельно)
triple :: (Num a) => a -> a
triple = (*3)

-- 6) Определение наибольшего из трёх заданных целых чисел (можно воспользоваться
-- стандартной двухаргументной функцией max).

max3 :: Ord a => a -> a -> a -> a
max3 x y z = max x $ max y z

{-
  Проверка:
> max3 87 34 209
209
> max3 22 28 30
30
> max3 12 25 (-7)
25
-}

-- 7) Дана температура в градусах Фаренгейта. Вычислить соответствующую температуру
-- в градусах Цельсия.
f2c :: Double -> Double
f2c = (/ 1.8) . subtract 32

{-
   8) Найти наибольший общий делитель двух целых чисел, пользуясь
      алгоритмом Евклида (псевдокод):
      НОД(a, 0) = a.
      НОД(a, b) = НОД(b, a mod b), если b ≠ 0;
-}

-- gcd' :: ???
gcd' :: Integral a => a -> a -> a
gcd' a 0 = a
gcd' a b = gcd' b (a `mod` b)

{-
   9) Найти значение функции f(x), определённой правилом:
             −x,   если x ≤ 0,
    f(x) =   x^2,  если 0 < x < 2,
             4,    если x ≥ 2.

-}

-- eval_f :: ???
eval_f :: (Ord a, Num a) => a -> a
eval_f x
    | x <= 0 = (-x)
    | x <  2 = x^2
    | otherwise = 4

-- 10) Функция, возвращающая название дня недели по его номеру (от 1 до 7),
--    если номер неправильный, генерируется исключение (функция error).
--    В реализации следует пользоваться сопоставлением с образцами.
dayOfWeek :: Int -> String
dayOfWeek 1 = "Sunday"
dayOfWeek 2 = "Monday"
dayOfWeek 3 = "Tuesday"
dayOfWeek 4 = "Wednesday"
dayOfWeek 5 = "Thursday"
dayOfWeek 6 = "Friday"
dayOfWeek 7 = "Saturday"

-- 11) Написать функцию, возвращающую текстовую характеристику ("hot", "warm", "cool", "cold")
-- по заданному значению температуры в градусах Цельсия.
describeTemperature :: Double -> String
describeTemperature t
    | t < 0  = "cold"
    | t < 15 = "cool"
    | t < 27 = "warm"
    | otherwise = "hot"

-- 12) Логическая операция xor (пользоваться стандартными логическими операциями не следует)

-- xor :: ???
xor :: Bool -> Bool -> Bool
xor True False = True
xor False True = True
xor _ _ = False


-- 13) Площадь круга по заданному радиусу

-- ???
circleArea :: Double -> Double
circleArea r = pi * r^2

{-
  14) Дан номер года (положительное целое число). Определить количество дней в этом году,
  учитывая, что обычный год насчитывает 365 дней, а високосный — 366 дней. Високосным
  считается год, делящийся на 4, за исключением тех годов, которые делятся на 100 и
  не делятся на 400 (например, годы 300, 1300 и 1900 не являются високосными,
  а 1200 и 2000 — являются).
-}

-- является ли год високосным
isLeapYear :: Int -> Bool
isLeapYear y = (y `mod` 4 == 0) && (y `mod` 100 /= 0) || (y `mod` 400 == 0)

-- количество дней в году
nDays :: Int -> Int
nDays y = if isLeapYear y then 366 else 365

-- Функция isLeapYear является вспомогательной, поэтому её можно определить внутри
-- nDays:

nDays' year
    | isLeap    = 366
    | otherwise = 365
    where
        isLeap = (year `mod` 4 == 0) && (year `mod` 100 /= 0) || (year `mod` 400 == 0)

-- 15) Простой тест, проверяющий, все ли требуемые функции определены:

test = if bothTrue False True || oneTrue False False || True `xor` True || f2c 80 > 0  then
         (gcd' 128 76 + sign (-6) + sign' 5 + max3 12 27 32 + nDays 2015 + nDays' 2015,
          triple (double (eval_f 1.5)),
          dayOfWeek 3 ++ " " ++ describeTemperature (f2c 100))
         else (0, 0, "")

 -- (766,15.1875,"Tuesday hot")

-- Выясните значение константы test в ghci, оно должно быть определено.
-- Этот тест не проверяет корректность определений функций.
