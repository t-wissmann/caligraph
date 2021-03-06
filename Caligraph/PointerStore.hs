{-# LANGUAGE TemplateHaskell #-}

module Caligraph.PointerStore (
    Ptr,
    PointerStore,
    empty,
    lookupUnsafe,
    lookupOrInsert,
    resolve,
) where

import Prelude hiding (lookup)
import Data.Hashable

import qualified Data.HashMap.Strict as M
import Control.Monad.State

import Lens.Micro
import Lens.Micro.TH
import Lens.Micro.Mtl

data Ptr = Ptr Int deriving (Eq,Ord)

--- (PointerStore a) makes it easy to enumerate values of type a to assign them
-- running integers.
data PointerStore d = PointerStore
    { _data2ptr :: M.HashMap d Int
    , _ptr2data :: M.HashMap Int d
    , _nextID :: Int
    }

makeLenses ''PointerStore

-- create an empty store
empty :: PointerStore a
empty = PointerStore M.empty M.empty 1024

-- If the given 'a' is in the PointerStore, return its index,
-- otherwise insert it and assign it the next free Int
lookupOrInsert :: Monad m => (Eq a, Hashable a) => a -> StateT (PointerStore a) m Ptr
lookupOrInsert a = do
    p2d <- use data2ptr
    case M.lookup a p2d of
        Just i ->
            return $ Ptr i
        Nothing -> do
            p <- use nextID
            nextID += 1
            data2ptr %= M.insert a p
            ptr2data %= M.insert p a
            return $ Ptr p

lookupUnsafe :: (Eq a, Hashable a) => PointerStore a -> a -> Ptr
lookupUnsafe ps a =
    case M.lookup a (_data2ptr ps) of
        Just p -> Ptr p
        Nothing -> error $ "Invalid item id"

resolve :: Monad m => Ptr -> StateT (PointerStore a) m a
resolve (Ptr p) = do
    d2p <- use ptr2data
    case M.lookup p d2p of
        Just a -> return a
        Nothing -> error $ "Invalid Ptr " ++ show p


