{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Accounts where

import Database.PostgreSQL.Simple
import Data.List.Zipper
import Control.Lens
import Filter

-- Accounts core API

-- Special relationship between AccLens and IOSelector in the alens_can_set field,
-- because IOSelector cannot communicate through lo1. An AccLens needs to know if
-- it can be updated or not, though the IOSelector actually implements the update.

data AccLens row = AccLens {
    _alens_get :: row -> String,
    _alens_can_set :: Bool,
    --_alens_set :: Maybe (row -> String -> row),
    _alens_name :: String
    }

makeLenses ''AccLens

instance Eq (AccLens row) where
    (==) a b = (a ^. alens_name) == (b ^. alens_name)
    (/=) a b = (a ^. alens_name) /= (b ^. alens_name)

class IOSelector a row where
    iosSelect :: a -> IO [row]
    iosInsert :: a -> IO row
    iosUpdate :: a -> row -> AccLens row -> String -> IO row
    iosDelete :: a -> row -> IO ()

class I1 a where 
    i1_select :: a -> IO a
    i1_insert :: a -> IO a
    i1_update :: a -> String -> IO a
    i1_delete :: a -> IO a
    i1_set_filters :: a -> [Filter] -> IO a
    i1_reset_filters :: a -> IO a
    i1_up :: a -> IO a
    i1_up_alot :: a -> IO a
    i1_down :: a -> IO a
    i1_down_alot :: a -> IO a
    i1_start :: a -> IO a
    i1_end :: a -> IO a
    i1_left :: a -> IO a
    i1_right :: a -> IO a

data LO1 row = LO1 {
    _lo1_zip_row :: Zipper row,
    _lo1_zip_lens :: Zipper (AccLens row),
    _lo1_zip_filters :: Zipper Filter
    }

makeLenses ''LO1

class HasLO1 a where
    getLO1 :: a b -> LO1 b

--data IOSelector row = IOSelector {
--    _ios_select :: Connection -> [Filter] -> IO [row],
--    _ios_insert :: Connection -> IO row,
--    _ios_update :: Connection -> row -> IO row,
--    _ios_delete :: Connection -> row -> IO ()
--    }

data SimpleIOSelector = SimpleIOSelector {
    _selectorConnection :: Connection
    }

makeLenses ''SimpleIOSelector

data ScopedIOSelector a = ScopedIOSelector {
    _scopedConnection :: Connection,
    _scopedMaybeScope :: Maybe a
    }

makeLenses ''ScopedIOSelector

class Scopeable a b where
    getMaybeScope :: a -> Maybe b -> Maybe b

class StringSettable a where
    setWithString :: String -> a -> a
