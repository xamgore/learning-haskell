import Parser
import SimpleParsers
import ParseNumbers
import Control.Applicative ((<|>))
import Control.Arrow ((***))
import Control.Monad
import Data.List (sortBy)


{-
   Определите тип для многочлена с вещественными коэффициентами.
-}
type    Member = (Int, Float)
newtype Poly   = Poly [Member]

instance Show Poly where
    show (Poly xs) = dropSign $ concatMap (add . (pow *** coef)) xs
        where
            dropSign s = (if s!!1 == '-' then "-" else "") ++ drop 3 s
            add (x, y) = y ++ x
            pow p
                | p == 0    = ""
                | p == 1    = "x"
                | otherwise = "x^" ++ show p
            coef c
                | c >= 0 = " + " ++ show c
                | c <  0 = " - " ++ show (-c)

{-
  Реализуйте парсер для многочленов (примеры в файле poly.txt).
-}

poly :: Parser Poly
poly = Poly . sort' <$> (removeSpaces >> many1 member)
    where sort' = sortBy (flip compare)

removeSpaces :: Parser ()
removeSpaces = Parser f
    where f str = [( (), filter (/= ' ') str )]

member :: Parser Member
member = do
    coef <- float
    pow  <- power
    return (pow, coef)

power :: Parser Int
power = (string "x^" >> integer) <|> (string "x" >> return 1) <|> return 0

float :: Parser Float
float = signed float'
    where
        float'        = (+) <$> justIntegral <*> maybeFraction
        justIntegral  = fromIntegral <$> natural
        maybeFraction = optional 0 (char '.' >> fraction)

{-
   Напишите функцию, которая вычисляет частное и остаток при делении многочлена на многочлен.
-}
divmod :: Poly -> Poly -> (Poly, Poly)
divmod = undefined

{-
   Напишите функцию, которая вычисляет наибольший общий делитель двух многочленов.
-}
poly_gcd :: Poly -> Poly -> Poly
poly_gcd = undefined

{-
   Напишите функцию, которая вычисляет наибольший общий делитель списка многочленов.
   Не забудьте воспользоваться свёрткой.
-}
poly_gcd_list :: [Poly] -> Poly
poly_gcd_list = undefined

{-
   Дан текстовый файл, в каждой строке которого записан один многочлен. Вычислите наибольший
   общий делитель многочленов из файла. Предусмотрите вывод соответствующего сообщения, если
   какая-либо строка файла имеет некорректный формат.
-}
poly_gcd_file :: FilePath -> IO (Either String Poly)
poly_gcd_file = undefined

{-
   В параметрах командной строки задано имя файла с многочленами. Найти их наибольший общий делитель.
   Предусмотреть корректную обработку ошибок (неправильное количество параметров командной строки,
   отсутствие файла, неверный формат файла и пр.).
-}
main = undefined
