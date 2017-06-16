module GrahamScan where

import Data.List (elemIndex, delete, sortBy)
import Data.Ord (compare, comparing)
import Control.Monad ()

-- 1. Определить тип Point для хранения информации о точке на вещественной плоскости.

data Point = Point { x :: Double, y :: Double } deriving (Eq)

instance Show Point where
    show pt = "(p " ++ show (floor $ x pt) ++ " " ++ show (floor $ y pt) ++ ")"

instance Ord Point where
    compare (Point x1 y1) (Point x2 y2) =
        if y1 == y2 then compare x2 x1 else compare y1 y2


p :: Double -> Double -> Point
p a b = Point { x = a, y = b }

{-
  2. Если заданы три точки a, b, c, можно рассматривать направление поворота от отрезка прямой,
  заключённого между точками a и b, к отрезку прямой, заключённому между точками b и c. Поворот
  может осуществляться влево, вправо или отрезки могут лежать на одной прямой — для представления
  этих трёх возможностей определить специальный тип Direction.
-}

data Direction = ToLeft | ToRight | OnLine deriving (Eq, Show)

direction :: Point -> Point -> Point -> Direction
direction (Point cx cy) (Point bx by) (Point ax ay) =
    case compare ((bx - ax)*(cy - by)) ((by - ay)*(cx - bx)) of
        GT -> ToLeft
        LT -> ToRight
        EQ -> OnLine

testDirection :: Bool
testDirection =
    direction (p 5 5) (p 0 5) (p 0 0) == ToRight &&
    direction (p 5 5) (p 5 0) (p 0 0) == ToLeft &&
    direction (p 4 4) (p 2 2) (p 0 0) == OnLine

{-
  3. Определить функцию, которая принимает список точек и вычисляет список направлений поворотов
  для каждых трёх последовательных точек. Например, для списка точек [a, b, c, d, e] она возвращает
  список поворотов для точек [a, b, c], [a, c, d] и [a, d, e]. При написании этой функции рекомендуется
  определить несколько вспомогательных функций.
-}

slide :: (p -> p -> d) -> [p] -> [d]
slide f zs = zipWith f zs (tail zs)

directions :: [Point] -> [Direction]
directions (z:zs) = slide (direction z) zs
directions _      = []

{-
  4. Пользуясь решениями предыдущих упражнений, реализовать алгоритм Грэхема нахождения выпуклой
  оболочки множества точек на вещественной плоскости. Описание алгоритма можно взять в английском
  (Graham scan) или русском разделах Википедии. Там же можно разобраться с тем, что именно называют
  выпуклой оболочкой (convex hull). Визуализация порядка работы алгоритма имеется на Youtube:
  http://www.youtube.com/watch?v=BTgjXwhoMuI
-}

clean :: [Point] -> [Point]
clean ps =
    case elemIndex ToRight $ directions ps of
        Just ix -> drop (ix + 1) ps
        Nothing -> ps

scan :: [Point] -> [Point]
scan (p1:p2:ps) = foldl push' [p2, p1] ps
    where push' stack point = point : clean (point : stack)


angle :: Point -> Point -> (Double, Double)
angle origin pt = (atan2 dx dy, dx)
    where dx = x pt - x origin; dy = y pt - y origin

grahamScan :: [Point] -> [Point]
grahamScan ps = scan $ origin : sorted
    where origin = minimum ps
          points = delete origin ps
          sorted = sortBy (comparing $ angle origin) points


{-
  5. Приведите несколько примеров работы функции graham_scan.
-}

-- example from wikipedia
grahamTest :: Bool
grahamTest = grahamScan [
        p 150 70,  p 20 160,  p 140 120, p 170 400, p 200 270, p 260 320,
        p 250 250, p 320 350, p 400 330, p 430 260, p 510 230, p 440 160,
        p 470 120
    ] == [
        p 470 120, p 510 230, p 400 330, p 170 400, p 20 160, p 150 70]
