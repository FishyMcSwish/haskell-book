module CH11.SumsAndProducts where

data GuessWhat = ChickenButt deriving (Show, Eq)

data Id a = MakeId a deriving (Eq, Show)

data Product a b = Product a b deriving (Eq, Show)

data Sum a b = 
  First a 
    | Second b
    deriving (Eq, Show)

data RecordProduct a b =
  RecordProduct { pfirst :: a
                , psecond :: b}
                deriving (Eq, Show)

newtype NumCow = NumCow Int deriving (Eq, Show)
newtype NumPig = NumPig Int deriving (Eq, Show)
data FarmHouse = FarmHouse NumCow NumPig deriving (Eq, Show)
type FarmHouse' = Product NumCow NumPig 

--FarmHouse and FarmHosue' are exactly the same, note that FarmHouse' is a type
--synonym for Product.

newtype NumSheep = NumSheep Int deriving (Eq, Show)
data BigFarmHouse = BigFarmHouse NumCow NumPig NumSheep deriving (Eq, Show)
type BigFarmHouse' = Product NumCow (Product NumPig NumSheep)


--Similar tricks with sums
--

type Name = String
type Age = Int
type LovesMud = Bool

type PoundsOfWool = Int

data CowInfo = CowInfo Name Age deriving (Eq, Show)
data PigInfo = PigInfo Name Age LovesMud deriving (Eq, Show)
data SheepInfo = SheepInfo Name Age PoundsOfWool deriving (Eq, Show)

data Animal = 
  Cow  CowInfo
    | Sheep SheepInfo 
    | Pig PigInfo 
    deriving (Eq, Show)


type Animal' = Sum CowInfo (Sum PigInfo SheepInfo)
