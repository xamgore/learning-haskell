{-# LANGUAGE TemplateHaskell #-}

import Reporter

{- Реализуйте функцию TemplateHaskell, которая по заданному имени типа
   возвращает количество его конструкторов данных. -}

data Point = Point Double Double

data Complex = Polar Double Double | Rectangular Double Double

data Shape = Circle Double | Square Double | Triangle Double Double Double

-- Должно быть True
$([d||])
test = $(countConstrs ''Point) == 1 && $(countConstrs ''Complex) == 2 && $(countConstrs ''Shape) == 3
