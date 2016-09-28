-- Даны типовые аннотации функций. Попытайтесь догадаться, что они делают,
-- дайте разумные имена и напишите их рекурсивные реализации (если вы
-- можете предложить несколько вариантов, реализуйте все):
-- 1) [a] -> Int -> a
get :: [a] -> Int -> a
get xs n = last $ take n xs

-- 2) Eq a => [a] -> a -> Bool


-- 3) [a] -> Int -> [a]

-- skip first n elements
drop'                       :: [a] -> Int -> [a]
drop' xs     n     | n <= 0 = xs
drop' []     _              = []
drop' (_:xs) n              = drop' xs (n-1)

-- skip everything except first n elements
take'                       :: [a] -> Int -> [a]
take' _      n     | n <= 0 = []
take' []     _              = []
take' (x:xs) n              = x : take' xs (n-1)

-- 4) a -> Int -> [a]

replicate'              :: a -> Int -> [a]
replicate' _ n | n <= 0 = []
replicate' x n          = x : replicate' x (n-1)

-- 5) [a] -> [a] -> [a]


-- 6) (Eq a, Ord a) => [a] -> [[a]]


-- 7) [a] -> [(Int, a)]


-- 8) Eq a => [a] -> [a]


-- 9) (a -> Bool) -> [a] -> a


-- 10) (a -> a) -> (a -> Bool) -> [a] -> [a]
