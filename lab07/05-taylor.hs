import Control.Monad.Writer

{-
   Организовать вычисление значений функций sin и cos, пользуясь рядами Тейлора
   и сохраняя каждое слагаемое в журнал посредством монады Writer. В тексте программы
   допускается только один вызов функции tell.
-}

sin' :: Double -> Int -> Writer [Double] Double
sin' x n = tell series >> return (sum series)
    where
        series = scanl f x [3, 5 .. (2 * fromIntegral n + 1)]
        f m s = (-1) * m * x * x / s / (s-1)


equals v = abs(sin v - fst (runWriter $ sin' v 1000)) < 0.0001

test_sin' = and [
        equals $ pi/2,
        equals $ pi/6,
        equals $ pi/3,
        equals 0
    ]
