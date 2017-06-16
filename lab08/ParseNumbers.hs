module ParseNumbers where

import Parser
import SimpleParsers
import Control.Applicative hiding (many, optional)
import Control.Monad


addition' = do
  n <- digit
  char '+'
  m <- digit
  return $ n + m

addition = digit >>= rest
  where
    rest m = fmap (+m) $ char '+' >> digit <|> return m


natural = foldl1 (\m n -> m *10 + n) `liftM` many1 digit


fraction :: Parser Float
fraction = fmap (/10.0) $ foldr (\m a -> fromIntegral m + a / 10.0) 0.0 `liftM` many1 digit


integer :: Parser Int
integer = signed natural

signed num = (*) <$> minus <*> num
    where minus = (char '-' >> return (-1)) <|> (char '+' >> return 1) <|> return 1


intList = bracket "[" "]" $ sepBy (token integer) (symbol ",")
