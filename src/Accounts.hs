{-# LANGUAGE TemplateHaskell #-}

module Accounts where

import Database.PostgreSQL.Simple
import Data.List.Zipper
import Control.Lens

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

-- I1 - Interface 1

data I1 a = I1 {
    _i1_select :: a -> IO a,
    _i1_insert :: a -> IO a,
    _i1_update :: String -> a -> IO a,
    _i1_delete :: a -> IO a,
    _i1_up :: a -> a,
    _i1_down :: a -> a,
    _i1_left :: a -> a,
    _i1_right :: a -> a
    }

makeLenses ''I1

-- LO1 - Logical Object 1

data LO1 row = LO1 {
    _lo1_zip_row :: Zipper row,
    _lo1_zip_lens :: Zipper (AccLens row)
    }

makeLenses ''LO1

-- ILO1 - Interactive Logical Object 1

data ILO1 a row = ILO1 {
    _ilo1_i1 :: I1 a,
    _ilo1_lo1f :: a -> LO1 row
    }

makeLenses ''ILO1

-- Implementation helps

-- IOSelector

data IOSelector row = IOSelector {
    _ios_select :: Connection -> IO [row],
    _ios_insert :: Connection -> IO row,
    _ios_update :: Connection -> row -> IO row,
    _ios_delete :: Connection -> row -> IO ()
    }

makeLenses ''IOSelector
