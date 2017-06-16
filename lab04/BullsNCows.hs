module BullsNCows where

import System.Random
import Data.Array.IO
import Control.Monad
import Data.List

{-
   Запрограммируйте игру «Быки и коровы» (https://ru.wikipedia.org/wiki/Быки_и_коровы)
   в варианте «компьютер загадывает — пользователь отгадывает».
-}


shuffle :: [a] -> IO [a]
shuffle xs = do
    ar <- newArr n xs
    forM [1..n] $ \i -> do
        j <- randomRIO (i,n)
        vi <- readArray ar i
        vj <- readArray ar j
        writeArray ar j vi
        return vj
    where
        n = length xs
        newArr :: Int -> [a] -> IO (IOArray Int a)
        newArr len =  newListArray (1, len)


chooseNum :: IO String
chooseNum = do
    digits <- shuffle ['0'..'9']
    return $ take 4 digits


exact :: Eq a => [a] -> [a] -> [a]
exact = ((map fst . filter snd) .) . zipWith (\x y -> (x, x == y))

fuzzy :: Eq a => [a] -> [a] -> [a]
fuzzy = intersect


describe :: String -> String -> IO ()
describe secret guess = do
    let ex = exact secret guess
    let fz = fuzzy secret guess \\ ex

    unless (null ex) $
        putStrLn $ "Угаданы на верных позициях: " ++ intersperse ' ' ex
    unless (null fz) $
        putStrLn $ "Угаданы на неверных позициях: " ++ intersperse ' ' fz
    when (null $ ex ++ fz) $
        putStrLn "Ничего не угадано"


game :: String -> IO ()
game secret = do
    guess <- getLine

    if guess == secret then
        putStrLn "Вы угадали, ура!"
    else do
        describe secret guess
        game secret


main :: IO ()
main = putStrLn "Я загадал число, попробуй его разгадать!"
    >> chooseNum >>= game
