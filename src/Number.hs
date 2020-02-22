module Number (Number(..)
    , inc
    , dec
) where

-- Peanos definition of Natural Numbers
data Number = Zero | Successor Number
    deriving (Show)

instance Eq Number where
    (==) Zero Zero = True
    (==) Zero (Successor _) = False
    (==) (Successor _) Zero = False
    (==) (Successor a) (Successor b) = (==) a b

inc :: Number -> Number
inc Zero = Successor Zero
inc (Successor a) = Successor(Successor(a))

dec :: Number -> Number
dec (Successor a) = a
dec _ = error "Can not decrement Zero, check condition guard"

