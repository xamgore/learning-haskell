{-
 1) Дан список слов.
  a) Преобразовать все слова к верхнему регистру.
  b) Извлечь из него подсписок слов заданной длины.
  c) Извлечь из него подсписок слов, начинающихся с заданной буквы.
  d) Извлечь из него подсписок слов, начинающихся и заканчивающихся
     с одной и той же буквы.
-}

import Data.Char

wsToUpper :: [String] -> [String]
wsToUpper = map $ map toUpper

wsByLength :: Int -> [String] -> [String]
wsByLength n = filter $ (n ==) . length

wsByFirstChar :: Char -> [String] -> [String]
wsByFirstChar ch = filter $ (ch ==) . head

wsSameChar :: [String] -> [String]
wsSameChar = filter $ \s -> head s == last s

{-
  2) Дана строка, содержащая слово и, возможно,
  лидирующие и последующие пробелы или знаки пунктуации.
  Очистить строку от лишних символов.

  В решении могут пригодиться функции dropWhile и takeWhile,
  а также предикаты из списка:
  https://www.haskell.org/hoogle/?hoogle=Char+-%3E+Bool

  Слова с дефисами должны обрабатываться корректно.
-}

cleanWord :: String -> String
cleanWord = twice $ reverse . dropWhile (not.isAlpha)
    where twice f = f . f

test_cleanWord = (all (=="слово")
                   $ map cleanWord ["слово", "  слово    ", "слово,", "!слово!", " -слово-"])
                 && cleanWord " чудо-юдо   " == "чудо-юдо"

-- 3) Дана строка, содержащая предложение на русском языке.
--    Посчитать количество содержащихся в ней n-буквенных слов.

wscount :: Int -> String -> Int
wscount n = length . filter ((n ==) . length) . map cleanWord . words

test_wscount = wscount 5 "Brick 123 quiz whangs 234 jumpy 3 veldt 444 fox!" == 3

-- 4) Дан текст в виде строки символов, содержащий среди прочего
--    числовые данные. Посчитать количество всех встречающихся в тексте чисел.

countNumbers :: String -> Int
countNumbers = length . filter (all isDigit) . words

test_countNumbers = countNumbers "Brick 123 quiz whangs 234 jumpy 3 veldt 444 fox!" == 4
