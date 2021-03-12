-- typecheck.hs
module TypeCheck where


data Person = Person Bool deriving Show  -- Originally deriving nothing.


printPerson :: Person -> IO ()
printPerson person = putStrLn (show person)


-- 2
data Mood = Blah
          | Woot deriving (Ord, Show)  -- Originally deriving only Show.


-- Originally had no instance of Eq.
instance Eq Mood where
  (==) Blah Blah = True
  (==) Woot Woot = True
  (==) _    _    = False


settleDown x = if x == Woot
                 then Blah
                 else x


{- 3: If you were able to get settleDown to type check:
    a) What values are acceptable inputs to that function?
    b) What will happen if you try to run settledown 9? Why?
    c) What will happen if you try to run Blah > Woot? Why? -}


-- 4
type Subject = String
type Verb = String
type Object = String


data Sentence =
  Sentence Subject Verb Object
  deriving (Eq, Show)


s1 = Sentence "dogs" "drool"
s2 = Sentence "Julie" "loves" "dogs"
