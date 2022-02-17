module Data.List.Ext ( imap
                     , ifilter'
                     , prior
                     ) where

prior :: (a -> a -> b) -> [a] -> [b]
prior op xs = zipWith op (tail xs) xs

imap :: (Int -> a -> b) -> [a] -> [b]
imap f xs = fmap (uncurry f) (zip [1..] xs)

ifilter' :: (Int -> a -> Bool) -> [a] -> [(Int, a)]
ifilter' p xs = filter (uncurry p) (zip [1..] xs)
