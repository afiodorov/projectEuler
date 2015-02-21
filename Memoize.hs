module Memoize(memoize) where

import Data.IORef (newIORef, readIORef, writeIORef)
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Map.Strict as Map

memoize :: Ord a => (a -> b) -> a -> b
memoize f = unsafePerformIO $ do
    r <- newIORef Map.empty
    return $ \ x -> unsafePerformIO $ do
        m <- readIORef r
        case Map.lookup x m of
            Just y  -> return y
            Nothing -> do
                    let y = f x
                    writeIORef r (Map.insert x y m)
                    return y
