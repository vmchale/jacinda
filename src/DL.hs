-- difference lists
module DL ( DL
          , cons, snoc
          , toList, empty
          ) where

infixr 9 `cons`
infixl 9 `snoc`

newtype DL a = DL ([a] -> [a])

toList = ($[]) . (\(DL x) -> x)

cons x (DL xs) = DL ((x:).xs)
snoc (DL xs) x = DL (xs.(x:))

empty = DL id

instance Functor DL where fmap f = foldr (cons.f) empty . toList

instance Semigroup (DL a) where
    (<>) (DL xs) (DL ys) = DL (xs.ys)
