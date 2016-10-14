{-# LANGUAGE ViewPatterns #-}

{-
  Дан текстовый файл numbers.txt, содержащий целые числа
  в диапазоне от 1 до 1000, разделённые пробелами и символами перевода строки. Определить
  количество различных чисел в нём, пользуясь для этого возможностями различных структур
  данных.
-}

import Data.List (nub)
import Data.Sequence (ViewL( (:<) ), (<|) )
import qualified Data.Sequence as S
import qualified Data.IntSet as Set
import Data.Array.IArray
import Control.Monad

nubSet :: Set.IntSet -> Int
nubSet = Set.size

nubList :: [Int] -> Int
nubList = length . nub

group :: Eq a => S.Seq a -> S.Seq (S.Seq a)
group (S.viewl -> x :< xs) = (x <| ys) <| group zs
    where (ys, zs) = S.spanl (x ==) xs
group _ = S.empty

nubSeq :: Ord a => S.Seq a -> Int
nubSeq = S.length . group . S.sort

nubArr :: Array Int Int -> Int
nubArr arr = sum (accumArray max 0 (1, 1000) elements :: Array Int Int)
    where elements = [(arr!i, 1) | i <- range (bounds arr)]


main :: IO ()
main = do
    let fname = "numbers.txt"
    content <- readFile fname

    let xs = map read $ concatMap words $ lines content
        n  = nubSet $ Set.fromList xs

        results = [
            nubList xs,
            nubSeq $ S.fromList xs,
            nubArr $ listArray (1, length xs) xs ]

    mapM_ print results
    when (any (/= n) results) $ putStrLn "Результаты не совпадают!"
