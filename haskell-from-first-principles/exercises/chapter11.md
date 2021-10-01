# Chapter 11: Algebraic Datatypes

### Dog types

```haskell
data Doggies a
    = Husky   a
    | Mastiff a
    deriving (Eq, Show)

data DogueDeBordeaux doge = DogueDeBordeaux doge
```

1. type constructor
2. `Doggies :: * -> *`
3. `Doggies String :: *`
4. `Husky 10 :: Num a => Doggies a`
5. `Husky (10 :: Integer) :: Doggies Integer`
6. `Mastiff "Scooby Doo" :: Doggies String`
7. both
8. `DogueDeBordeaux :: doge -> DogueDeBordeaux doge`
9. `DogueDeBordeaux "doggie!" :: DogueDeBordeaux String`

### Vehicles

```haskell
data Price = Price Integer deriving (Eq, Show)

data Manufacturer
    = Mini
    | Mazda
    | Tata
    deriving (Eq, Show)

data Airline
    = PapuAir
    | CatapulsR'Us
    | TakeYourChancesUnited
    deriving (Eq, Show)

data Vehicle
    = Car Manufacturer Price
    | Plane Airline Price
    deriving (Eq, Show)

myCar    = Car Mini  (Price 14000)
urCar    = Car Mazda (Price 20000)
clownCar = Car Tata  (Price 7000)
doge     = Plane PapuAir (Price 419200000)

-- 1. What is the type of myCar?
-- myCar :: Vehicle

-- 2. Define the following functions.
isCar :: Vehicle -> Bool
isCar v = case v of
  (Car _ _) -> True
  _         -> False

isPlane :: Vehicle -> Bool
isPlane = not . isCar

areCars :: [Vehicle] -> [Bool]
areCars = map isCar

-- 3. Write a function to tell us the manufacturer of a piece of data.
getManu :: Vehicle -> Manufacturer
getManu (Car m _) = m

{- 4. Given that we're returning the Manufacturer, what will happen if you use
this on Plane data?

It would create an exception because not every condition has been accounted for.
Instead of a Vehicle -> Manufacturer, we could use a Maybe, like in the getManu'
function I've written below. Another option might be to add a new data NoManu to
the Manufacturer type, which would provide more information than just giving a
Nothing value. -}
getManu' :: Vehicle -> Maybe Manufacturer
getManu' v = case v of
    (Car m _) -> Just m
    _         -> Nothing

{- 5. All right. Let's say you decide to add the size of the plane as an
argument to the Plane constructor. Add that to your datatypes in the appropriate
places, and change your data and functions appropriately. -}
```
