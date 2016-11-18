import Parser
import SimpleParsers
import ParseNumbers
import Control.Applicative ((<|>))

{- Напишите парсер для вещественных чисел. -}
float :: Parser Float
float = signed float'
    where
        float'        = (+) <$> justIntegral <*> maybeFraction
        justIntegral  = fromIntegral <$> natural
        maybeFraction = optional 0 (char '.' >> fraction)

{-
  Напишите парсер для представления комплексных чисел,
  записываемых в виде вещественной и мнимой части через запятую
  в круглых скобках, например, "(2.3, 1)".
-}

complex :: Parser (Float, Float)
complex = do
    token $ char '('
    r <- token float
    token $ char ','
    i <- token float
    token $ char ')'
    return (r, i)

{-
  Напишите парсер для списка комплексных чисел (разделитель — точка с запятой),
  заключённого в квадратные скобки.
-}
complexList :: Parser [(Float, Float)]
complexList = bracket "[" "]" $ sepBy complex (symbol ";")

{-
  Модифицируйте предыдущий парсер таким образом, чтобы в исходной строке
  могли встречаться как комплексные числа, так и вещественные (мнимая часть
  при этом должна считаться равной нулю).
-}
complexList2 :: Parser [(Float, Float)]
complexList2 = complexList' ";"

complexList' :: String -> String -> Parser [(Float, Float)]
complexList' sep = bracket "[" "]" $ sepBy complexOrFloat (symbol sep)
    where complexOrFloat = complex <|> flip (,) 0.0 <$> token float

{-
   Модифицируйте предыдущий парсер таким образом, чтобы компоненты списка
   разделялись запятой, а не точкой запятой. Постарайтесь реализовать
   требуемое с помощью вспомогательных парсеров, допускающих повторное применение.
-}
complexList3 :: Parser [(Float, Float)]
complexList3 = complexList' ","
