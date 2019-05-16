module Lib (
    dateFromString,
    give, truncate, one, and, get,
    konst, scaleK,
    Currency (..),
    Date, Contract
) where
    
import Prelude hiding (and, or, truncate)
data Date = Date {day :: Int, month :: Int, year :: Int} | Infinity deriving Eq

instance Show Date where
    show Date {
        day = d, month = m, year = y
    } = (show d) ++ " " ++ (show m) ++ " " ++ (show y)
    show Infinity = "INF"

instance Ord Date where
    compare Date {
        day = a_day, month = a_month, year = a_year
    } Date {
        day = b_day, month = b_month, year = b_year
    } = case compare a_year b_year of
            EQ -> case compare a_month b_month of
                    EQ -> case compare a_day b_day of
                            d -> d
                    m -> m
            y -> y

    compare Infinity _ = GT
    compare _ Infinity = LT

dateFromString :: String -> Date
dateFromString str = let [day, mon, yr] = map read $ words str :: [Int] in
    Date { day = day, month = mon, year = yr }

data Direction = ToMe | FromMe deriving (Eq, Show, Read)

reverseDirection :: Direction -> Direction
reverseDirection ToMe = FromMe
reverseDirection FromMe = ToMe

data Currency = RUB | USD | GBP | Any deriving (Eq, Show, Read)

data Contract = Contract {
    horizon :: Date,
    amount :: Double,
    currency :: Currency,
    direction :: Direction
} | And Contract Contract
  | Or Contract Contract
  | Then Contract Contract
  | Get Contract
   deriving (Show, Eq)

data Obs a = Obs a

instance Num a => Num (Obs a) where
    (Obs a) + (Obs b) = Obs (a + b)
    (Obs a) * (Obs b) = Obs (a * b)
    abs (Obs a) = Obs (abs a)
    signum (Obs a) = Obs (signum a)
    fromInteger = Obs . fromInteger
    negate (Obs a) = Obs (negate a)

konst :: a -> Obs a
konst = Obs

scale :: Obs Double -> Contract -> Contract
scale (Obs x) c@Contract{ amount = a } = c { amount = a * x }
scale coeff (And c1 c2) = And (scale coeff c1) (scale coeff c2)
scale coeff (Or c1 c2) = Or (scale coeff c1) (scale coeff c2)
scale coeff (Then c1 c2) = Then (scale coeff c1) (scale coeff c2)
scale coeff (Get c1) = Get (scale coeff c1)

scaleK :: Double -> Contract -> Contract
scaleK x c = scale (konst x) c

zero = Contract Infinity 0 Any ToMe

one :: Currency -> Contract
one k = Contract Infinity 1 k ToMe

give :: Contract -> Contract
give c@Contract {
    direction = dir
} = c { direction = reverseDirection dir }
give (And c1 c2) = And (give c1) (give c2)
give (Or c1 c2) = Or (give c1) (give c2)
give (Then c1 c2) = Then (give c1) (give c2)
give (Get c1) = give c1

truncate :: Date -> Contract -> Contract
truncate t c@Contract {
    horizon = hor
} = c { horizon = min t hor }
truncate t (And c1 c2) = And (truncate t c1) (truncate t c2)
truncate t (Or c1 c2) = Or (truncate t c1) (truncate t c2)

and :: Contract -> Contract -> Contract
and c1 c2 = And c1 c2

or :: Contract -> Contract -> Contract
or c1 c2 = Or c1 c2

then_ :: Contract -> Contract -> Contract
then_ c1 c2 = Then c1 c2

get :: Contract -> Contract
get c = Get c