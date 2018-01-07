{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Accounts where

import Database.PostgreSQL.Simple
import Data.List.Zipper
import Control.Lens

-- Accounts core API

-- Special relationship between AccLens and Selector in the alens_can_set field,
-- because IOSelector cannot communicate through lo1. An AccLens needs to know if
-- it can be updated or not, though the Selector actually implements the update.

data AccLens row = AccLens {
    _alens_get :: row -> String,
    _alens_can_set :: Bool,
    _alens_name :: String
    }

makeLenses ''AccLens

instance Eq (AccLens row) where
    (==) a b = (a ^. alens_name) == (b ^. alens_name)
    (/=) a b = (a ^. alens_name) /= (b ^. alens_name)

class Selector m a row where
    iosSelect :: a -> m [row]
    iosInsert :: a -> m row
    iosUpdate :: a -> row -> AccLens row -> String -> m row
    iosDelete :: a -> row -> m ()

class I1 m a where 
    i1_select :: a -> m a
    i1_insert :: a -> m a
    i1_update :: a -> String -> m a
    i1_delete :: a -> m a
    i1_up :: a -> m a
    i1_up_alot :: a -> m a
    i1_down :: a -> m a
    i1_down_alot :: a -> m a
    i1_start :: a -> m a
    i1_end :: a -> m a
    i1_left :: a -> m a
    i1_right :: a -> m a

data LO1 row = LO1 {
    _lo1_zip_row :: Zipper row,
    _lo1_zip_lens :: Zipper (AccLens row)
    }

makeLenses ''LO1

class HasLO1 a where
    getLO1 :: a b -> LO1 b

data SimpleSelector = SimpleSelector {
    _selectorConnection :: Connection
    }

makeLenses ''SimpleSelector

data ScopedSelector a = ScopedSelector {
    _scopedConnection :: Connection,
    _scopedScope :: a
    }

makeLenses ''ScopedSelector

class Scopeable a b where
    getScope :: a -> b -> b

class StringSettable a where
    setWithString :: String -> a -> a
