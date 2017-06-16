{-
  Для тестирования программ, работающих с файловой системой, часто необходимо
  достаточно большое дерево каталогов с файлами. Реализуйте случайный генератор
  такого дерева, управляемый набором параметров (минимальная и максимальная ширина
  и глубина дерева, количество и размеры файлов, что-нибудь ещё). В качестве идеи
  для архитектуры приложения используйте пример с подсчётом количества файлов в
  дереве (count.hs). Этот же пример можно использовать для тестирования
  разработанного приложения.
-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
import System.Environment
import System.Directory
import System.FilePath
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.State
import Control.Applicative

data AppConfig = AppConfig {
      cfgMaxDepth :: Int
    } deriving (Show)

data AppState = AppState {
      curDepth :: Int,
      curPath :: FilePath
    } deriving (Show)

type AppLog = [(FilePath, Int)]

newtype MyApp a = MyA {
      runA :: ReaderT AppConfig (StateT AppState (WriterT AppLog IO)) a
    } deriving (Functor, Applicative, Monad,
                MonadIO,
                MonadReader AppConfig,
                MonadWriter [(FilePath, Int)],
                MonadState AppState)

runMyApp :: MyApp a -> Int -> FilePath -> IO (a, AppLog)
runMyApp app maxDepth path =
    let config = AppConfig maxDepth
        state = AppState 0 path
    in runWriterT (evalStateT (runReaderT (runA app) config) state)

listDirectory' :: FilePath -> IO [String]
listDirectory' = liftM (filter notDots) . getDirectoryContents
    where notDots p = p /= "." && p /= ".."

constrainedCount :: MyApp ()
constrainedCount = do
    st <- get
    let depth = curDepth st
    maxDepth <- cfgMaxDepth `liftM` ask

    when (depth < maxDepth) $ do
        let path = curPath st
        contents <- liftIO $ listDirectory' $ path

        -- generate directories & files
        replicateM_ 10 $ do
            newName <- show <$> (randomRIO (1, 1000))
            writeFile (path </> newName </> ".txt") "kek"
            createDirectory (path </> newName)

        forM_ contents $ \name -> do
            let newPath  = path </> name
            let newDepth = depth + 1
            isDir <- liftIO $ doesDirectoryExist newPath

            when isDir $ do
            put $ st {stCurDepth = newDepth, stCurPath = newPath}
            constrainedCount

main = do
    [d, p] <- getArgs
    (_, xs) <- runMyApp constrainedCount (read d) p
    print xs
