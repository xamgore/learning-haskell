-- Пользуясь средствами монады ST, запрограммируйте любой алгоритм сортировки массива.

import Data.STRef
import Control.Monad.ST
import Control.Monad

import Data.Array
import Data.Array.ST
import Data.Array.MArray

moveUp :: (Ord e, Ix i, Integral i) => i -> i -> STArray s i e -> ST s ()
moveUp i j arr | i < j = do
     x <- readArray arr i
     y <- readArray arr j

     when (x > y) $ do
         writeArray arr i y
         writeArray arr j x


bubbleST :: (Ord e, Ix i, Integral i) => STArray s i e -> ST s (STArray s i e)
bubbleST arr = do
    (li, ri) <- getBounds arr

    forM_ [li .. ri] $ \_ ->
        forM_ [li .. ri - 1] $ \j ->
            moveUp j (j + 1) arr

    return arr


sort :: (Ord a) => [a] -> [a]
sort xs = elems $ runSTArray $ do
    arr <- newListArray (0, length xs - 1) xs
    bubbleST arr
    return arr

test_sort = sort [5, 1, 3, 4, 2] == [1..5]
