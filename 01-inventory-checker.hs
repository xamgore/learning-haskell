import Control.Monad
import Data.List
import Data.Maybe (mapMaybe)
import System.Environment

{-
   Дан текстовый файл (inventory.txt) с перечислением всей имеющейся на складе
   лёгкой брони. Сформируйте список имеющихся полных комплектов брони одного
   вида (kind).

   Указание: всюду следует использовать монадический синтаксис (операции >>= и >>,
   функции ap, liftM и другие функции монадической обработки данных, использование
   блока do и аппликативного стиля не допускается).
-}

data ArmorType = Shield | Helmet | Gauntlets | Boots | Cuirass
   deriving (Show, Read, Eq, Ord, Enum, Bounded)
data ArmorKind = Chitin | Hide | Leather | Elven | Scaled | Glass | ImperialLight
   deriving (Show, Read, Eq, Ord, Enum, Bounded)

data ArmorItem = ArmorItem ArmorKind ArmorType
   deriving (Show, Eq)
data ArmorKit = ArmorKit ArmorKind [ArmorType]
   deriving (Show, Eq)

aType (ArmorItem _ t) = t
aKind (ArmorItem k _) = k

loadInventory :: FilePath -> IO [ArmorItem]
loadInventory f = readFile f >>= return . map (toArmor . words) . lines
    where toArmor [t, k] = ArmorItem (read t) (read k)

buildArmorKit :: ArmorKind -> [ArmorItem] -> Maybe ArmorKit
buildArmorKit kind items = (isFull . getTypes . hasKind) items >>= return . ArmorKit kind
    where hasKind  = filter ((kind ==) . aKind)
          getTypes = sort . nub . map aType
          isFull s = if length s == 5 then Just s else Nothing

buildKits :: [ArmorItem] -> [ArmorKit]
buildKits items = mapMaybe (`buildArmorKit` items) kinds
    where kinds = nub $ map aKind items


main :: IO ()
main = (head `liftM` getArgs) >>= loadInventory >>= print . buildKits
