module System.IO.Streams.Ext ( imap
                             , ifilter
                             , scan
                             , prior
                             , dupStream
                             ) where

import           Control.Monad     ((<=<))
import           Data.IORef        (modifyIORef', newIORef, readIORef)
import qualified System.IO.Streams as Streams

-- prior :: (a -> a -> a) -> [a] -> [a]
-- prior op xs = zipWith op (tail xs) xs

prior :: (a -> a -> a) -> Streams.InputStream a -> IO (Streams.InputStream a)
prior op xs = do
    (xsϵ, xsϵ') <- dupStream xs
    shifted <- Streams.drop 1 xsϵ
    Streams.zipWith op shifted xsϵ'

-- FIXME: don't do this; do something better for zips! (like, in-memory)
dupStream :: Streams.InputStream a -> IO (Streams.InputStream a, Streams.InputStream a)
dupStream = Streams.unzip <=< Streams.map (\x -> (x, x)) -- aka join (,) 🤓

scan :: (b -> a -> b)
     -> b
     -> Streams.InputStream a
     -> IO (Streams.InputStream b)
scan op seed inp = do
    acc <- newIORef seed
    Streams.mapM (\x -> do
        { y <- readIORef acc
        ; modifyIORef' acc (`op` x)
        ; pure (y `op` x)
        }) inp

imap :: (Int -> a -> b)
     -> Streams.InputStream a
     -> IO (Streams.InputStream b)
imap f inp = do
    ix <- newIORef 1
    Streams.mapM (\a -> do
        { ixϵ <- readIORef ix
        ; modifyIORef' ix (+1)
        ; pure (f ixϵ a)
        }) inp

ifilter :: (Int -> a -> Bool)
        -> Streams.InputStream a
        -> IO (Streams.InputStream a)
ifilter p inp = do
    ix <- newIORef 1
    Streams.filterM (\x -> do
        { ixϵ <- readIORef ix
        ; modifyIORef' ix (+1)
        ; pure (p ixϵ x)
        }) inp
