import Control.Monad
import Data.List
import System.Environment

{-
	Дан текстовый файл (inventory.txt) с перечислением всей имеющейся на складе
	лёгкой брони. Сформируйте список имеющихся полных комплектов брони одного
	вида (kind). Указание: в решении рекомендуется пользоваться монадическими
	операциями всюду, где только возможно.
-}

data ArmorType = Shield | Helmet | Gauntlets | Boots | Cuirass
	deriving (Show, Read, Eq, Ord, Enum, Bounded)
data ArmorKind = Chitin | Hide | Leather | Elven | Scaled | Glass | ImperialLight
	deriving (Show, Read, Eq, Ord, Enum, Bounded)
data ArmorItem = ArmorItem ArmorKind ArmorType
	deriving (Show, Eq)
data ArmorKit = ArmorKit ArmorKind [ArmorType]
	deriving (Show, Eq)
	
loadInventory :: FilePath -> IO [ArmorItem]
loadInventory fname = do
	xs <- readFile fname
	let readItem x:y:xs = ArmorItem (read x) (read y)
	in return xs >>= lines >>= words >>= map readItem

buildArmorKit :: ArmorKind -> [ArmorItem] -> Maybe ArmorKit
buildArmorKit kind items = items >>= filter isKindOfItem >>= map takeType >>= makeKit >>= return
	where isKindOfItem kind (ArmorItem x _) = x == kind
			takeType (ArmorItem _ y) = y
			makeKit kind xs = ArmorKit kind xs

buildKits :: [ArmorItem] -> Maybe [ArmorKit]
buildKits items = [Chitin ..] >>= (\k -> buildArmorKit k items)

main = (head `liftM` getArgs) >>= loadInventory >>= buildKits >>= print