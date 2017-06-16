import Control.Monad
import Data.Either

{-
  Модифицируйте имеющуюся реализацию задачи о канатоходце (лекция 9) следующим образом:
  1) реализуйте загрузку входных данных из файла следующего вида:
       R 2
       L 3
       R -1
       B
       L 1
     и вычисление соответствующего им результата (в решении может пригодиться
     функция foldr (<=<) return — проверьте её тип для получения подсказки);
  2) замените монаду Maybe на Either String так, чтобы в случае падения канатоходца
     можно было получить информацию о его причинах (нарушение баланса и в какую
     сторону или банан на канате);
  3) реализуйте операцию landBoth, поддерживающую одновременное (атомарное) приземление
     птиц на оба конца шеста, и внесите соответствующие изменения в другие функции;
  5) реализуйте операцию unlandAll (одновременный вылет всех птиц с шеста) и внесите
     соответствующие изменения в другие функции;
  4) организуйте масштабное тестирование.
-}

type Birds = Int
type State = (Birds, Birds)

data Threat  = L | R | B deriving (Eq, Show)
type Problem = (Threat, Int)

type Err = String

-- [(R,2),(L,3),(R,-1),(B,0),(L,1)]
readProblems :: String -> [Problem]
readProblems = map (parse . words) . lines
    where parse ["B"]    = (B, 1)
          parse ["L", x] = (L, read x)
          parse ["R", x] = (R, read x)

runFile :: FilePath -> IO (Either Err State)
runFile f = run . readProblems <$> readFile f
    where run ps = pipe (map land ps) (0, 0)
          pipe   = foldl (>=>) return


balance = 3

updatePole :: State -> Either Err State
updatePole (l, r) = if unbalanced then Left err else Right (l, r)
  where unbalanced = abs (l - r) > balance
        err        = if l > r then "Imbalance on the left"
                              else "Imbalance on the right"

landLeft :: Birds -> State -> Either Err State
landLeft n (left, right) = updatePole (left + n, right)

landRight :: Birds -> State -> Either Err State
landRight n (left, right) = updatePole (left, right + n)

landBoth :: Birds -> Birds -> State -> Either Err State
landBoth n m (left, right) = updatePole (left + n, right + m)

unlandAll :: State -> Either Err State
unlandAll = const $ Right (0, 0)

banana :: State -> Either Err State
banana = const $ Left "Banana!"

land :: Problem -> State -> Either Err State
land (L, n) = landLeft n
land (R, n) = landRight n
land (B, _) = banana


tests = all test [1..3]
  where
    test 1 = isLeft (return (0, 0) >>= landLeft 1 >>= landRight 4
              >>= landLeft (-1) >>= landRight (-2))
    test 2 = (return (0, 0) >>= landRight 2 >>= landLeft 2 >>= landRight 2) == Right (2, 4)
    test 3 = isLeft (return (0, 0) >>= landLeft 1 >>= banana >>= landRight 1)
