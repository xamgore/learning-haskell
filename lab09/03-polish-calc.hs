{-
   Представленный на лекции вычислитель выражений в обратной польской нотации
   не проверяет корректность заданного выражения, выдавая, к примеру, следующие
   очевидно неправильные ответы:

   ghci> evalRPN "* 1"
   1
   ghci> evalRPN "+ * 2 4"
   4
   ghci> evalRPN "* * *"
   *** Exception: 01-polish-calc.hs:10:15-43: Non-exhaustive patterns in lambda

   1. Переработайте реализацию вычислителя таким образом, чтобы в случае ошибки ответ
   не выводился. Воспользуйтесь в решении монадой Maybe, совместив её с монадой State
   с помощью преобразователя монад.

   2. Добавьте к вычислителю поддержку переменных. Значения переменных должны
   задаваться в командной строке, а доступ к ним должен осуществляться средствами
   монады Reader.

   3. Добавьте к вычислителю подсчёт количества операций со стеком (монада Writer
   с журналом типа Sum Int).
-}


import Data.Char
import Data.Maybe
import Data.Monoid
import qualified Data.Map as M
import Control.Monad
import Control.Monad.Writer
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import System.Environment

type Stack = [Int]
type Mem   = M.Map String String

push :: Int -> MaybeT (StateT Stack (ReaderT Mem (Writer (Sum Int)))) ()
push x = tell (getSum 1) >> lift get >>= lift . put . (x:)

pop :: MaybeT (StateT Stack (ReaderT Mem (Writer (Sum Int)))) Int
pop = do
    tell (getSum 1)
    st <- lift get
    guard (not $ null st)
    lift $ put (tail st)
    return $ head st

empty = M.fromList [("a", "777")]

-- evalRPN :: Mem -> String -> (Stack, Sum Int)
evalRPN mem xs = runWriter $ runReaderT (execStateT (runMaybeT app) []) mem
  where
    app = mapM step $ words xs
    step "+" = processTops (+)
    step "*" = processTops (*)
    step n | all isAlpha n = lift (lift ask) >>= push . read . fromJust . M.lookup n
    step n = push (read n)
    processTops op = op `liftM` pop `ap` pop >>= push

main = do
    mem <- M.fromList . zop <$> getArgs
    forever (getLine >>= print . evalRPN mem)

zop (x:y:ys) = (x, y) : zop ys
zop _ = []
