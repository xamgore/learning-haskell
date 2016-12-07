import Parser
import SimpleParsers
import ParseNumbers
import Control.Applicative ((<|>))
import Control.Arrow ((***), second, first)
import Control.Monad
import qualified Data.Map as M


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
poly = Poly <$> (removeSpaces >> many1 member)

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
-- divmod :: Poly -> Poly -> (Poly, Poly)
-- Poly xs `divmod` Poly ys = foldl (Poly xs, Poly [(0, 0)]) divide ys
--     where
--         diff ([(qp, qc):_], [(dp, dc):_]) = [(qp - dp), (qc / dc)]
--         divide (Poly quot, Poly rem) d =
--             let dif = diff quot d in

normalize :: Poly -> Poly
normalize (Poly ms) = Poly $ zero $ clean $ M.toDescList $ M.fromListWithKey (const (+)) ms
    where clean     = filter ((/= 0.0) . snd)
          zero ms   = if null ms then [(0, 0.0)] else ms

add :: Poly -> Poly -> Poly
Poly x `add` Poly y = normalize $ Poly $ x ++ y

mult :: Poly -> Float -> Poly
Poly x `mult` y = Poly $ map (second (y *)) x

liftPow :: Poly -> Poly -> Poly
Poly x `liftPow` Poly [(p, c)] = (Poly $ map (first (+ p)) x) `mult` c

{-
   Напишите функцию, которая вычисляет наибольший общий делитель двух многочленов.
-}
poly_gcd :: Poly -> Poly -> Poly
poly_gcd (Poly a) (Poly b) = foldl divide (Poly a, Poly [(0, 0)]) b
    where divide = undefined

{-
   Напишите функцию, которая вычисляет наибольший общий делитель списка многочленов.
   Не забудьте воспользоваться свёрткой.
-}
poly_gcd_list :: [Poly] -> Poly
poly_gcd_list = foldl1 poly_gcd

{-
   Дан текстовый файл, в каждой строке которого записан один многочлен. Вычислите наибольший
   общий делитель многочленов из файла. Предусмотрите вывод соответствующего сообщения, если
   какая-либо строка файла имеет некорректный формат.
-}
poly_gcd_file :: FilePath -> IO (Either String Poly)
poly_gcd_file = readFile f >>= return . Right . poly_gcd_list . map (parse poly) . lines

{-
   В параметрах командной строки задано имя файла с многочленами. Найти их наибольший общий делитель.
   Предусмотреть корректную обработку ошибок (неправильное количество параметров командной строки,
   отсутствие файла, неверный формат файла и пр.).
-}
main = head <$> getArgs >>= poly_gcd_file
