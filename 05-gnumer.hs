{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, InstanceSigs #-}

import GHC.Float

{- Реализуйте все необходимые экземпляры класса GNum,
   чтобы этот файл стал компилируемым. -}

class GNum a b where
  type SumTy a b :: *
  (+++) :: a -> b -> SumTy a b

instance GNum Double Int where
    type SumTy Double Int = Double
    a +++ b = fromIntegral b + a

instance GNum Integer Float where
    type SumTy Integer Float = Double  -- hi float
    a +++ b = fromIntegral a + float2Double b

instance GNum Double Double where
    type SumTy Double Double = Double
    (+++) = (+)


test = (a +++ k) +++ (n +++ z)
  where
    a = 1.5 :: Double
    k = 5 :: Int
    n = 100 :: Integer
    z = 0.1 :: Float
