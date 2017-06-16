{-
   Напишите программу обработки квадратных матриц (на массивах) со следующими возможностями:
   1) чтение матрицы из тестового файла;
   2) запись матрицы в текстовый файл;
   3) сумма матриц;
   4) произведение матриц.

  Задание на обработку может выглядеть, к примеру, следующим образом (здесь вычисляется матричное
  выражение (A + B) * C):

    LOAD a FROM matr1.txt
    LOAD b FROM matr2.txt
    LOAD c FROM matr3.txt
    CALC d AS PLUS a b
    CALC e AS MULT d c
    SAVE d TO matr4.txt

   Параметром командной строки должно быть имя файла со сценарием указанного или подобного ему вида.
-}

import qualified Data.Map.Strict as M
import System.Environment
import Data.Maybe (fromMaybe)
import Data.Array

type Var = String
type Val = Array (Int, Int) Double
type Namespace = M.Map Var Val


msum :: (Ix i, Num a) => Array (i, i) a -> Array (i, i) a -> Array (i, i) a
msum x y = array resultBounds [
    (i, x!i + y!i) | r <- range(lr, ur), c <- range(lc, uc), let i = (r, c) ]
    where
        bx@((lr,lc),(ur,uc)) = bounds x
        resultBounds
            | bx == bounds y = bx
            | otherwise = error "matSum: incompatible bounds"

mmult :: (Ix i, Num a) => Array (i, i) a -> Array (i, i) a -> Array (i, i) a
mmult x y
    | x1 /= y0 || x1' /= y0'  = error "range mismatch"
    | otherwise               = array ((x0, y1), (x0', y1')) l
    where
        ((x0, x1), (x0', x1')) = bounds x
        ((y0, y1), (y0', y1')) = bounds y
        ir = range (x0, x0')
        jr = range (y1, y1')
        kr = range (x1, x1')
        l  = [((i, j), sum [x!(i, k) * y!(k, j) | k <- kr]) | i <- ir, j <- jr]


main :: IO ()
main = do
    args <- getArgs
    let nm = M.fromList []
    if null args then repl nm else run (head args) nm

run :: FilePath -> Namespace -> IO ()
run file space = readFile file >>= flip process space . lines
    where process (x:xs) nm = parse (words x) nm >>= process xs
          process [] _      = return ()


repl :: Namespace -> IO ()
repl space = getLine >>= flip parse space . words >>= repl

parse :: [String] -> (Namespace -> IO Namespace)
parse (cmd : args) =
    case cmd of
        "LOAD" -> load (head args) (last args)
        "SAVE" -> save (head args) (last args)
        "CALC" -> calc (head args) (take 3 $ drop 2 args)
        _      -> return

load :: Var -> FilePath -> Namespace -> IO Namespace
load var file space = insert . read' <$> readFile file
    where insert x  = M.insert var x space
          read' txt = listArray ((1,1),(rows,cols)) vals
            where vals = map read $ words txt
                  rows = length $ lines txt
                  cols = length $ words $ head $ lines txt

save :: Var -> FilePath -> Namespace -> IO Namespace
save var file space = maybe err ok (M.lookup var space)
    where ok res = writeFile file (printArray res) >> return space
          err    = error "Variable not found"

calc :: Var -> [String] -> Namespace -> IO Namespace
calc var [op, x, y] space = return $ M.insert var (get x × get y) space
    where (×) = case op of "PLUS" -> msum; "MULT" -> mmult
          get = fromMaybe err . flip M.lookup space
          err = error "Variable not found"

calc _ _ space = return space


printArray arr = let (_,(r, c)) = bounds arr in
  unlines [unwords [show (arr ! (x, y)) | x <- [1..r]] | y <- [1..c]]
