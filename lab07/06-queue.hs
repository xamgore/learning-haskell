import Data.List (elemIndices)
import Control.Monad.State
import Control.Arrow

{-
  1) Пользуясь монадой State, реализовать функции для работы с очередью: enqueue, dequeue, top.
  2) Смоделируйте алгоритм FIFO замещения страниц виртуальной памяти: на вход алгоритму подаются
     номера используемых страниц виртуальной памяти, на выходе — моменты времени, в которые
     необходимо замещать загруженную ранее страницу. Постарайтесь написать программу так, чтобы
     её пользователю было понятно, что в ней происходит.
-}

type Que a = [a]


enq :: a -> State (Que a) ()
enq x = state (mempty &&& (x:))

deq :: State (Que a) a
deq = state (last &&& init)

top :: State (Que a) a
top = state (last &&& id)

has :: Eq a => a -> State (Que a) Bool
has x = state (elem x &&& id)


--- fifo

load :: Int -> State (Que Int) Bool
load page = do
    notFound <- not <$> has page
    when notFound $ deq >> enq page
    return notFound

fifo :: [Int] -> [Int]
fifo queries = timeOf pageFaults
    where
        timeOf       = elemIndices True
        pageFaults   = evalState (traverse load queries) initialState
        initialState = [-1, -1, -1]


testFifo = fifo [5, 6, 5, 7, 8, 5] == [0, 1 {--}, 3, 4, 5]
