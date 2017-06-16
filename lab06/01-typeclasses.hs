{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

import Data.Char (ord, chr)

{-
   Класс типов Binary предназначен для получения явного
   двоичного представления некоторого типа данных.
-}

class Binary a where
  toBinary :: a -> [Bool]
  fromBinary :: [Bool] -> Maybe a

{-
   1. Реализуйте экземпляры класса типов Binary для следующих типов:
      * Integer
      * Char
      * Bool
      * [Bool]
      * String
-}

instance Binary Integer where
    toBinary i | i >= 0 = reverse $ map ((/= 0) . snd) $ takeWhile notZero $ iterate (divmod2 . fst) (divmod2 i)
          where divmod2 = flip divMod 2; notZero (x, y) = x /= 0 || y /= 0

    fromBinary [] = Nothing
    fromBinary bs = Just $ snd $ foldr f (0, 0) integers
        where integers = map (\b -> if b then 1 else 0) bs
              f x (pow, s) = (pow + 1, x * 2^pow + s)

instance Binary Char where
    toBinary   = toBinary . toInteger . ord
    fromBinary = fmap (chr . fromInteger) . fromBinary

instance Binary Bool where
    toBinary = (:[])
    fromBinary [] = Nothing
    fromBinary [True]  = Just True
    fromBinary [False] = Just False

instance Binary [Bool] where
    toBinary   = id
    fromBinary = Just


{-
   2. Определите класс типов Listable с двумя функциями:
   toList :: a -> [a]
   fromList :: [a] -> a
-}

class Listable a where
    toList   ::  a -> [a]
    fromList :: [a] -> a

{-
  3. Объявите экземпляры класса типов Listable для следующих типов:
     * String - строка разбивается по пробелам на список слов.
     * Integer - любое целое число разбивается на список цифр.
-}

instance Listable String where
    toList   = words
    fromList = unwords

instance Listable Integer where
    toList 0 = [0]
    toList n = reverse $ map snd $ takeWhile notZero $ iterate (divmod10 . fst) (divmod10 n)
          where divmod10 = flip divMod 10; notZero (x, y) = x /= 0 || y /= 0
    fromList = snd . foldr (\x (p, s) -> (p * 10, x * p + s)) (1, 0)
