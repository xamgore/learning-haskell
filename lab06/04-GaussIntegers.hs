{-# LANGUAGE GADTs #-}

import Data.Monoid
import Data.Foldable

{-
   Это упражнение посвящено гауссовым целым числам. Всю необходимую информацию
   можно найти на соответствующей странице Википедии.
-}

data GaussInteger a = GI a a deriving (Eq)


-- 1. Определите экземпляры классов типов Eq и Show ("re + i*im") для типа GaussInteger.

instance Show a => Show (GaussInteger a) where
    show (GI r i) = show r ++ " + i·" ++ show i


-- 2. Определите следующие операции:
plus :: Integral a => GaussInteger a -> GaussInteger a -> GaussInteger a
plus (GI r i) (GI r' i') = GI (r + r') (i + i')

mult :: Integral a => GaussInteger a -> GaussInteger a -> GaussInteger a
mult (GI r i) (GI r' i') = GI (r*r' - i*i') (r*i' + r'*i)

norm :: Integral a => GaussInteger a -> a
norm (GI r i) = r*r + i*i


-- 3. Определите две обёртки (newtype) и напишите для обёрток два экземпляра
--    класса Monoid — для сложения и для умножения.

newtype GiSum  a = GiSum  { getGiSum  :: GaussInteger a }
newtype GiMult a = GiMult { getGiMult :: GaussInteger a }

instance Integral a => Monoid (GiSum a) where
    mempty  = GiSum $ GI 0 0
    mappend (GiSum x) (GiSum y) = GiSum $ x `plus` y

instance Integral a => Monoid (GiMult a) where
    mempty = GiMult $ GI 1 0
    mappend (GiMult x) (GiMult y) = GiMult $ x `mult` y


{-
  4. Определите для  GaussInteger экземпляр класса типов Functor, соответствующий
     следующему примеру:

     ghci> fmap (+1) (GI 5 10)
     GI 6 11
-}

instance Functor GaussInteger where
    fmap f (GI r i) = GI (f r) (f i)

{-
  5. Реализуйте следующие функции:
-}

sumGI :: (Integral a, Foldable t) => t (GaussInteger a) -> GaussInteger a
sumGI = getGiSum . foldMap GiSum

productGI :: (Integral a, Foldable t) => t (GaussInteger a) -> GaussInteger a
productGI = getGiMult . foldMap GiMult
