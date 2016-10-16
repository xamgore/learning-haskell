{-# LANGUAGE EmptyDataDecls #-}

module Drunkard where

import Data.Ord
import Data.List
import Data.Maybe

{-
  1. Определить типы данных, необходимые для представления игральной карты в игре «Пьяница»,
  учитывая, что всего в колоде 52 карты.
-}

data Suit = Spades | Hearts | Diamonds | Clubs deriving (Eq)

instance Show Suit where
    show Spades   = "♠"
    show Hearts   = "♥"
    show Diamonds = "♦"
    show Clubs    = "♣"

data Value = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten |
        Jack | Queen | King | Ace deriving (Eq, Ord, Enum)

instance Show Value where
    show Ace   = "A"
    show King  = "K"
    show Queen = "Q"
    show Jack  = "J"
    show v     = show $ fromJust(elemIndex v [Two .. Ten]) + 2

data Card = Card Value Suit

instance Show Card where
    show (Card v s) = show v ++ show s

of' :: Suit -> Suit
of' = id

card :: Value -> (Suit -> Suit) -> Suit -> Card
card = (.) . Card

-- 2. Определить функцию, проверяющую, что две переданные ей карты одной масти.

sameSuit :: Card -> Card -> Bool
sameSuit (Card _ s) (Card _ t) = s == t

{-
  3. Определить функцию, проверяющую, что переданная ей первой карта старше второй
  (масть в игре «Пьяница» игнорируется). Возвращённое значение EQ означает, что обе
  карты одинакового старшинства.
-}

valueOf :: Card -> Value
valueOf (Card v _) = v

beats :: Card -> Card -> Ordering
c1 `beats` c2 = comparing valueOf c1 c2

{-
  4. Определить функцию, которая по паре списков карт возвращает новую пару списков карт
  с учетом правил игры «Пьяница» (один раунд игры):
    * из вершин списков берутся две карты и добавляются в конец того списка, карта из
      которого старше оставшейся;
    * если первые взятые карты совпадают по достоинству, то из списков берутся и
      сравниваются следующие две карты (и так до тех пор, пока не будет определён победитель
      раунда).
-}

type State = ([Card], [Card])

gameRound :: State -> Maybe State
gameRound (xs, ys) = step result
    where
        rounds    = zip [1..] (zipWith beats xs ys)
        result    = find ((/= EQ) . snd) rounds
        win count = flat $ take count $ zip xs ys
        flat      = foldr (\(a,b) ls -> a:b:ls) []

        step res = case res of
            Just (count, GT) -> Just (drop count xs ++ win count, drop count ys)
            Just (count, LT) -> Just (drop count xs, drop count ys ++ win count)
            _                -> Nothing

{-
  5. Определить функцию, которая по паре списков возвращает количество раундов, необходимых
  для завершения игры (одна из колод оказывается пустой), и номер победителя.
-}

data Winner = First | Second | Nobody deriving (Show)


game :: State -> (Winner, Int, [State])

game s@([], _)  = (Second, 0, [s])
game s@(_, [])  = (First,  0, [s])

game s = case gameRound s of
            Just nextState -> next $ game nextState
            Nothing        -> case compare (length $ fst s) (length $ snd s) of
                                EQ -> (Nobody, 0, [s])
                                GT -> (First,  1, [s])
                                LT -> (Second, 1, [s])
        where
            next (win, rounds, his) = (win, rounds + 1, s : his)

{-
  6. Приведите здесь результаты как минимум пяти запусков функции game (в каждом списке
  изначально должно быть не менее 10 карт).
-}

-- λ> gameRound ([card Six of' Spades, card King of' Spades], [card Six of' Hearts, card Ace of' Hearts])
-- Just ([],[6♠,6♥,K♠,A♥])

-- λ> gameRound ([], [card Six of' Spades, card King of' Spades, card Six of' Hearts, card Ace of' Hearts])
-- Nothing

{-
  7 (необязательное упражнение). Реализуйте версию функции game, которая помимо результатов
  игры возвращает запись всех ходов (карты, выкладываемые по ходу игры для сравнения).
-}

-- game возвращает третий элемент кортежа, историю ходов

{-
  8 (необязательное упражнение). При выполнении функций из упражнений 4 и 5 возможно
  зацикливание. Чтобы его избежать, можно предусмотреть максимальное количество повторений
  (для раундов и ходов в рамках одного раунда). Подумайте, как обнаружить факт зацикливания
  в функции 4? Можно ли применить такой же подход в функции 5? Что нужно возвращать в случае
  обнаружения факта зацикливания? Измените соответствующим образом типовые аннотации и
  напишите безопасные по отношению к зацикливанию версии функций game_round и game.
-}

-- готово
