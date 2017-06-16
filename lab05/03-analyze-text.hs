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
import Data.Ord (comparing)
import Data.List (sort, sortBy, group, minimumBy)
import Data.Function
import Data.Char (toLower, isAlpha)
import System.Environment

main :: IO ()
main = do
    args <- getArgs

    case head args of
        "bi"   -> readFile (args!!1) >>= printTop 10 . topBigrams
        "tri"  -> readFile (args!!1) >>= printTop 10 . topTrigrams
        "top"  -> readFile (args!!1) >>= printTop 50 . topWords
        "mark" -> readFile (args!!1) >>= print . topPunMark
        _  -> do
            putStrLn "  top  <file> -- print top 50 words"
            putStrLn "  bi   <file> -- print top 10 bigrams"
            putStrLn "  tri  <file> -- print top 10 trigrams"
            putStrLn "  mark <file> -- print the most common punct mark"


topBigrams :: String -> [(String, Int)]
topBigrams = mostCommon . bigrams . words . clear
    where
        clear     = map (\c -> if isAlpha c then c else ' ')
        bigrams   = concatMap (combine . shift)
        combine   = uncurry (zipWith (+++))
        shift     = (,) <$> tail <*> id
        c1 +++ c2 = [c2, c1]

topTrigrams :: String -> [(String, Int)]
topTrigrams = mostCommon . trigrams . words . clear
    where
        clear     = map (\c -> if isAlpha c then c else ' ')
        trigrams  = concatMap (combine . shift) . filter ((>3) . length)
        combine (a,b,c) = zipWith3 toList a b c
        shift     = (,,) <$> (tail . tail) <*> tail <*> id
        toList c1 c2 c3 = [c3, c2, c1]


isPunctuationMark :: Char -> Bool
isPunctuationMark = flip elem ['.', ',', ':', ';', '!', '?', '-', '—', '\'', '"', '(', ')']

topPunMark :: String -> String
topPunMark = head . minimumBy (flip $ comparing length) .
        group . sort . words . map (\c -> if isPunctuationMark c then c else ' ')


printTop :: Int -> [(String, Int)] -> IO ()
printTop n xs = mapM_ printPair $ take n xs
    where printPair (s, c) = putStrLn $ unwords [s, show c]

topWords :: String -> [(String, Int)]
topWords = mostCommon . filter isWord . map clean . words . map toLower

mostCommon :: [String] -> [(String, Int)]
mostCommon = sortBy (flip compare `on` snd) .
        M.toAscList . M.fromListWith (+) . flip zip [1, 1..]

isWord :: String -> Bool
isWord s = all (\ c -> isAlpha c || c == '-') s && not (null s)

clean :: String -> String
clean = twice $ reverse . dropWhile (not.isAlpha)
    where twice f = f . f
