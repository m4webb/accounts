{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Accounts where

import Database.PostgreSQL.Simple
import Data.List.Zipper
import Control.Lens
import Filter

-- Accounts core API

-- AccLens - Accounts Lens

data AccLens row = AccLens {
    _alens_get :: row -> String,
    _alens_set :: Maybe (row -> String -> row),
    _alens_name :: String
    }

makeLenses ''AccLens

instance Eq (AccLens row) where
    (==) a b = (a ^. alens_name) == (b ^. alens_name)
    (/=) a b = (a ^. alens_name) /= (b ^. alens_name)

class I1 a where 
    i1_select :: a -> IO a
    i1_insert :: a -> IO a
    i1_update :: a -> String -> IO a
    i1_delete :: a -> IO a
    i1_set_filters :: a -> [Filter] -> IO a
    i1_reset_filters :: a -> IO a
    i1_up :: a -> IO a
    i1_down :: a -> IO a
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

class IOSelector a row where
    iosSelect :: a -> IO [row]
    iosInsert :: a -> IO row
    iosUpdate :: a -> row -> IO row
    iosDelete :: a -> row -> IO ()

data SimpleIOSelector = SimpleIOSelector {
    _selectorConnection :: Connection
    }

makeLenses ''SimpleIOSelector
