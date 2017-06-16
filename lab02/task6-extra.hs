-- Даны типовые аннотации функций. Попытайтесь догадаться, что они делают,
-- дайте разумные имена и напишите их рекурсивные реализации (если вы
-- можете предложить несколько вариантов, реализуйте все):

-- 1) [a] -> Int -> a

get      :: [a] -> Int -> a
get xs n = last $ take n xs

-- 2) Eq a => [a] -> a -> Bool

elem'          :: Eq a => [a] -> a -> Bool
elem' []     _ = False
elem' [x]    y = x == y
elem' (x:xs) y = (x == y) || elem' xs y

-- 3) [a] -> Int -> [a]

drop'                   :: [a] -> Int -> [a]
drop' xs     n | n <= 0 = xs
drop' []     _          = []
drop' (_:xs) n          = drop' xs (n-1)

take'                   :: [a] -> Int -> [a]
take' _      n | n <= 0 = []
take' []     _          = []
take' (x:xs) n          = x : take' xs (n-1)

-- 4) a -> Int -> [a]

replicate'              :: a -> Int -> [a]
replicate' _ n | n <= 0 = []
replicate' x n          = x : replicate' x (n-1)

-- 5) [a] -> [a] -> [a]

(+++)           :: [a] -> [a] -> [a]
(+++) []     ys = ys
(+++) (x:xs) ys = x : (xs +++ ys)

-- 6) (Eq a, Ord a) => [a] -> [[a]]

group' :: (Eq a) => [a] -> [[a]]
group' [] = []
group' (x:xs) = f x (group' xs)
    where
        f x [] = [[x]]
        f x (r@(y:_):rs)
            | x == y    = (x : r) : rs
            | otherwise = [x]: r  : rs

-- 7) [a] -> [(Int, a)]

enum xs = zip [1..length xs] xs

-- 8) Eq a => [a] -> [a]

-- TODO nub

-- 9) (a -> Bool) -> [a] -> a

findFirst          :: (a -> Bool) -> [a] -> a
findFirst _ []     = error "Not found"
findFirst p (x:xs) = if p x then x else findFirst p xs

-- 10) (a -> a) -> (a -> Bool) -> [a] -> [a]

mapWhile            :: (a -> a) -> (a -> Bool) -> [a] -> [a]
mapWhile _ _ []     = []
mapWhile f p (x:xs) = if p x then f x : mapWhile f p xs else x : xs
