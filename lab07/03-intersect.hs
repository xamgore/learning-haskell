import Control.Monad

{- Пользуясь списком как монадой, вычислите пересечение заданных списков -}
inter :: Eq a => [a] -> [a] -> [a]
inter xs ys = [x | x <- xs, x `elem` ys]

intersect :: Eq a => [[a]] -> [a]
intersect = foldr1 inter
