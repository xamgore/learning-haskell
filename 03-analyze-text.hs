{-
  Соберите следующую информацию по текстовому файлу kapitan.txt:

  1) самый часто используемый знак препинания;
  2) 50 наиболее часто используемых в тексте слов (с указанием количества использований);
  3) частоты символов, биграмм и триграмм (вывести соответствующую информацию для
     наиболее часто встречающихся);
  4) количества использованных предлогов, местоимений и имён собственных.

  Постарайтесь использовать в решении наиболее подходящие структуры данных, избегайте повторные
  вычисления и заведомо неэффективные операции. Проверьте написанную программу на трёх других
  текстах того же или большего размера. Сравните результаты.
-}

import qualified Data.Map.Strict as M
import Data.List (sortBy)
import Data.Function
import Data.Char (toLower, isAlpha)
import System.Environment

main :: IO ()
main = do
    args <- getArgs

    case head args of
        "top" -> readFile (args!!1) >>= printTop50
        _  -> do
            putStrLn "  top <file> -- print top 50 words"




printTop50 :: String -> IO ()
printTop50 = mapM_ (\(s, c) -> putStrLn $ unwords [s, show c]) . take 50 . topWords

topWords :: String -> [(String, Int)]
topWords =
    sortBy (flip compare `on` snd) . M.toAscList .
    M.fromListWith (+) . flip zip (repeat 1) .
    filter isWord . map clean . words . map toLower

isWord :: String -> Bool
isWord s = all (\ c -> isAlpha c || c == '-') s && not (null s)

clean :: String -> String
clean = twice $ reverse . dropWhile (not.isAlpha)
    where twice f = f . f
