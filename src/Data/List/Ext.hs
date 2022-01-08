module Data.List.Ext ( imap
                     , ifilter
                     , prior
                     ) where

prior :: (a -> a -> a) -> [a] -> [a]
prior op xs = zipWith op (tail xs) xs

imap :: (Int -> a -> b) -> [a] -> [b]
imap f xs = fmap (uncurry f) (zip [1..] xs)

ifilter :: (Int -> a -> Bool) -> [a] -> [a]
ifilter p xs = snd <$> filter (uncurry p) (zip [1..] xs)