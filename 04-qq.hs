{- Разработайте QuasiQuoter, который делает данный файл компилируемым. -}

{-# LANGUAGE QuasiQuotes #-}

import Worder

data Name = Name String String deriving (Show)

list2Name :: [String] -> Name
list2Name [x, y] = Name x y

namesDB :: [Name]
namesDB = map list2Name [wrds|Ivanov Ivan
                              Petrov Petr
                              Sidorova Maria
                              Kozlov Konstantin |]
