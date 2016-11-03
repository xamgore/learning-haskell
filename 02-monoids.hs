-- 1. Реализуйте instance Monoid для типа Xor, в котором mappend выполняет операцию xor.

newtype Xor = Xor { getXor :: Bool }
    deriving (Eq, Show)

instance Monoid Xor where
    mempty                  = Xor False
    mappend (Xor x) (Xor y) = Xor (x /= y)


{-
  2. Реализуйте instance Monoid для Maybe' a так, чтобы mempty не был равен Maybe' Nothing.
  Нельзя накладывать никаких дополнительных ограничений на тип a, кроме указанных в условии.
-}

newtype Maybe' a = Maybe' { getMaybe :: Maybe a }
    deriving (Eq, Show)

instance Monoid a => Monoid (Maybe' a) where
    mempty                        = Maybe' $ Just mempty
    mappend (Maybe' x) (Maybe' y) = Maybe' (mappend x y)

{-
    λ> (Maybe' $ Just [1, 2]) `mappend` (Maybe' $ Just [3, 4])
    Maybe' {getMaybe = Just [1,2,3,4]}
    λ> (Maybe' $ Just [1, 2]) `mappend` (mempty)
    Maybe' {getMaybe = Just [1,2]}
-}
