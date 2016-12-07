{-
   Модифицируйте представленное на лекции решение задачи о запросе пароля,
   удовлетворяющего требованиям по стойкости, следующим образом:
   - в командной строке задаются ограничения (минимальная длина, наличие букв,
     наличие цифр, наличие знаков пунктуации);
   - к стеку монад добавляется монада Reader, и с её помощью организуется
     доступ к ограничениям в функции isValid;
   - все попытки ввода пароля протоколируются средствами монады Writer.
-}

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer
import System.Environment

import Data.Char

data Config = Config Int Bool Bool Bool

main = do
    [minLen, alpha, numbers, other] <- getArgs
    let cfg = Config (read minLen) (read alpha) (read numbers) (read other)
    story <- runWriterT $ runReaderT (runMaybeT askPassword) cfg
    print $ snd story


askPassword :: MaybeT (ReaderT Config (WriterT [String] IO)) ()
askPassword = do
    value <- msum $ repeat getValidPassword
    liftIO $ putStrLn "Сохранение в базе данных..."

getValidPassword :: MaybeT (ReaderT Config (WriterT [String] IO)) String
getValidPassword = do
  liftIO $ putStrLn "Введите новый пароль:"
  passw <- liftIO getLine
  lift $ lift $ tell [passw]
  valid <- lift $ isValid passw
  guard valid
  return passw

isValid :: String -> (ReaderT Config (WriterT [String] IO)) Bool
isValid s = do
    (Config minLen alpha numbers other) <- ask
    return $ and [
        length s >= minLen,
        not alpha   || any isAlpha  s,
        not numbers || any isNumber s,
        not other   || any isPunctuation s ]
