-- chapter07; see Shapes module as well.
module Chapter07 where

import qualified Data.Map as Map


data Person = Person { firstName   :: String
                     , lastName    :: String
                     , age         :: Int
                     , height      :: Float
                     , phoneNumber :: String
                     , flavor      :: String
                     } deriving (Eq, Ord, Read, Show)


data Car = Car { company :: String
               , model   :: String
               , year    :: Int
               } deriving (Show)


data Vector a = Vector a a a deriving (Show)


data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
           deriving (Bounded, Enum, Eq, Ord, Read, Show)


data LockerState = Taken | Free deriving (Eq, Show)


infixr 5 :-:
data List a = Empty | a :-: (List a) deriving (Eq, Ord, Read, Show)


data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show)

instance Functor Tree where
    fmap f EmptyTree           = EmptyTree
    fmap f (Node x left right) = Node (f x) (fmap f left) (fmap f right)


data TrafficLight = Red | Yellow | Green

instance Eq TrafficLight where
    Red    == Red    = True
    Yellow == Yellow = True
    Green  == Green  = True
    _      == _      = False

instance Show TrafficLight where
    show Red    = "Red Light"
    show Yellow = "Yellow Light"
    show Green  = "Green Light"


class YesNo a where
    yesno :: a -> Bool

instance YesNo Int where
    yesno 0 = False
    yesno _ = True

instance YesNo [a] where
    yesno [] = False
    yesno _  = True

instance YesNo Bool where
    yesno = id

instance YesNo (Maybe a) where
    yesno (Just _) = True
    yesno Nothing  = False

instance YesNo (Tree a) where
    yesno EmptyTree = False
    yesno _         = True

instance YesNo TrafficLight where
    yesno Red = False
    yesno _   = True


type Name        = String
type PhoneNumber = String
type PhoneBook   = [(Name, PhoneNumber)]

type AssocList k v = [(k, v)]

type Code      = String
type LockerMap = Map.Map Int (LockerState, Code)


tellCar :: Car -> String
tellCar (Car {company = c, model = m, year = y}) =
    "This " ++ c ++ " " ++ m ++ " was made in " ++ show y ++ "."


vplus :: (Num a) => Vector a -> Vector a -> Vector a
vplus (Vector i j k) (Vector l m n) = Vector (i + l) (j + m) (k + n)


dotProd :: (Num a) => Vector a -> Vector a -> a
dotProd (Vector i j k) (Vector l m n) = i * l + j * m + k * n


vmult :: (Num a) => Vector a -> a -> Vector a
vmult (Vector i j k) m = Vector (i * m) (j * m) (k * m)


mikeD  = Person { firstName   = "Michael"
                , lastName    = "Diamond"
                , age         = 43
                , height      = 187.0
                , phoneNumber = "555-1234"
                , flavor      = "Chocolate"
                }

adRock = Person { firstName   = "Adam"
                , lastName    = "Horovitz"
                , age         = 41
                , height      = 174.1
                , phoneNumber = "555-2345"
                , flavor      = "Vanilla"
                }

mca    = Person { firstName   = "Adam"
                , lastName    = "Yauch"
                , age         = 44
                , height      = 180.5
                , phoneNumber = "555-3456"
                , flavor      = "Butterscotch"
                }


phoneBook :: PhoneBook
phoneBook =
    [ ("betty",   "555-2938")
    , ("betty",   "342-2492")
    , ("bonnie",  "452-2928")
    , ("patsy",   "493-2928")
    , ("patsy",   "943-2929")
    , ("patsy",   "827-9162")
    , ("lucille", "205-2928")
    , ("wendy",   "939-8282")
    , ("penny",   "853-2492")
    , ("penny",   "555-2111")
    ]


inPhoneBook :: Name -> PhoneNumber -> PhoneBook -> Bool
inPhoneBook name pnumber pbook = elem (name, pnumber) pbook


lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup lockerNumber map = case Map.lookup lockerNumber map of
    Nothing -> Left $ "Locker " ++ show lockerNumber ++ " doesn't exist!"
    Just (state, code) ->
        if state /= Taken then Right code
                          else Left $ "Locker " ++ show lockerNumber
                                                ++ " is already taken!"


lockers :: LockerMap
lockers = Map.fromList
    [ (100, (Taken, "ZD39I"))
    , (101, (Free,  "JAH3I"))
    , (103, (Free,  "IQSA9"))
    , (105, (Free,  "QOTSA"))
    , (109, (Taken, "893JJ"))
    , (110, (Taken, "99292"))
    ]


infixr 5 ^++
(^++) :: List a -> List a -> List a
Empty      ^++ ys = ys
(x :-: xs) ^++ ys = x :-: (xs ^++ ys)


singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree


treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree           = singleton x
treeInsert x (Node a left right)
    | x == a = Node x left right
    | x <  a = Node a (treeInsert x left) right
    | x >  a = Node a left (treeInsert x right)


treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree           = False
treeElem x (Node a left right)
    | x == a = True
    | x <  a = treeElem x left
    | x >  a = treeElem x right


yesnoIf :: (YesNo y) => y -> a -> a -> a
yesnoIf yesnoVal yesResult noResult = 
    if yesno yesnoVal
        then yesResult
        else noResult
