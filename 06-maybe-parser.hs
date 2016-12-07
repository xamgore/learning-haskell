{-
   Тип Parser может быть определён следуюшим образом:
-}

import Control.Applicative
import Control.Monad
import Data.Maybe

newtype Parser a = Parser { apply :: String -> Maybe (a, String) }

{-
   Определите экземпляры классов Functor, Applicative, Alternative, Monad и MonadPlus
   для типа Parser в этом случае:
-}

instance Functor Parser where
  fmap = liftM

instance Applicative Parser where
    pure  = return
    (<*>) = ap

instance Alternative Parser where
    (<|>) = mplus
    empty = mzero

instance Monad Parser where
  return x = Parser $ \s -> Just (x, s)
  p >>= q  = Parser (apply p >=> \(res, s) -> apply (q res) s)
  fail _ = Parser $ const Nothing

instance MonadPlus Parser where
  mzero = Parser $ const Nothing
  p `mplus` q = Parser (\s -> let ps = apply p s in if isNothing ps then apply q s else ps)
