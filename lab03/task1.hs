import Data.List
import Data.Ord

-- 1) Пользуясь функцией find и инструкцией case, определить, имеются ли в данном
--    целочисленном списке положительные нечётные элементы.

hasPosOdd :: [Integer] -> Bool
hasPosOdd xs = case find (\x -> x >= 0 && odd x) xs of
        Nothing -> False
        _       -> True

-- 2) Дан список. Найти в нём наименьший нечётный элемент.

smallest_odd :: (Eq a, Ord a, Integral a) => [a] -> Maybe a
smallest_odd = foldl step Nothing
  where
    -- step         :: Maybe a -> a -> Maybe a
    step Nothing  x = Just x
    step (Just m) x = Just $ min m x

-- 3) Пользуясь функциями sortBy и comparing, упорядочить заданный список точек на плоскости по
--    убыванию ординат


data Point = Point {x :: Double, y :: Double}
  deriving (Show, Eq, Ord)

sortByY :: [Point] -> [Point]
sortByY = sortBy $ comparing $ negate . y

-- 4) Дополнить тип для геометрических фигур треугольником и дополнить реализацию
--    функций вычисления площади и периметра.

dist (Point x1 y1) (Point x2 y2) =
    sqrt $ (x1 - x2)**2 + (y1 - y2)**2

type Radius = Double

data Shape = Circle Point Radius | Rectangle Point Point | Triangle Point Point Point
                deriving (Show)

area :: Shape -> Double
area (Circle _ r) = pi * r ^ 2

area (Rectangle (Point x1 y1) (Point x2 y2)) =
    abs (x2 - x1) * abs (y2 - y1)

area (Triangle p1 p2 p3) =
    sqrt $ p * (p - a) * (p - b) * (p - c)
        where
            a = dist p1 p2
            b = dist p1 p3
            c = dist p2 p3
            p = (a + b + c) / 2

perimeter :: Shape -> Double
perimeter (Circle _ r) = 2 * pi * r

perimeter (Rectangle (Point x1 y1) (Point x2 y2)) =
    (x2 - x1) * 2 + (y2 - y1) * 2

perimeter (Triangle p1 p2 p3) = dist p1 p2 + dist p2 p3 + dist p1 p3

-- 5) Реализовать аналоги стандартных функций head, tail, length, last, map и filter
--    для объявленного на лекции типа List a.

data List a = Nil | Cons a (List a)

head' :: List a -> a
head' Nil        = error "empty list"
head' (Cons x _) = x

tail' :: List a -> List a
tail' Nil         = error "empty list"
tail' (Cons _ xs) = xs

length' :: List a -> Int
length' Nil         = 0
length' (Cons _ xs) = 1 + length' xs

last' :: List a -> a
last' Nil = error "empty list"
last' (Cons x Nil) = x
last' (Cons _ xs)  = last' xs

map' :: (a -> b) -> List a -> List b
map' _ Nil         = Nil
map' f (Cons x xs) = Cons (f x) (map' f xs)

filter' :: (a -> Bool) -> List a -> List a
filter' _ Nil         = Nil
filter' p (Cons x xs) = if p x then Cons x $ filter' p xs else filter' p xs

-- 6) Пользуясь функцией unfoldr, реализовать построение произвольной числовой последовательности,
--    каждый элемент которой вычисляется через два предыдущих.

iterate2 :: (a -> a -> a) -> a -> a -> [a]
iterate2 next a1 a2 = a1 : a2 : unfoldr produce (a1, a2)
    where
        produce (f1, f2) = let f3 = next f1 f2 in Just (f3, (f2, f3))
