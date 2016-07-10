{-# LANGUAGE MultiParamTypeClasses #-}

module Accounts where

import Database.PostgreSQL.Simple

class I1 a where
    i1_select :: a -> IO a
    i1_update :: a -> String -> IO a
    i1_insert :: a -> IO a
    i1_delete :: a -> IO a
    i1_up :: a -> a
    i1_down :: a -> a
    i1_left :: a -> a
    i1_right :: a -> a

data LO1 row = LO1
    (ZipList row)
    (ZipList (IOLens row))
    deriving (Show)

class I1 a => ILO1 a row where
    ilo1_state :: a -> LO1 row

-- lens has field-level scope, selector has row-level scope
    
data IOLens row = IOLens {
    lens_get :: row -> String,
    lens_set :: Maybe (row -> String -> Connection -> IO row),
    lens_name :: String
    }

instance Show (IOLens row) where
    show lens = "IOLens " ++ (lens_name lens)

data IOSelector row = IOSelector {
    ios_select :: Connection -> IO [row],
    ios_insert :: Connection -> IO row,
    ios_delete :: Connection -> row -> IO ()
    }

instance Show (IOSelector row) where
    show selector = "IOSelector"

-- ZipList

data ZipList a = ZipList a [a] [a]

zl_up :: ZipList a -> ZipList a
zl_up (ZipList curr_item prev_item:b_items [a_items]) = ZipList prev_item  b_items curr_items:a_items
zl_up (ZipList curr_item [] a_items) = ZipList curr_item [] a_items

zl_down :: ZipList a -> ZipList a
zl_down ZipList curr_item b_items next_item:a_items = ZipList next_item  curr_item:b_items a_items
zl_down ZipList curr_item b_items [] = ZipList curr_item b_items []

zl_unzip :: ZipList a -> ZipList a
zl_unzip ZipList item b_items [] = ZipList item b_items []
zl_unzip ziplist = zl_unzip <*> zl_down ziplist

zl_rezip :: ZipList a -> ZipList a
zl_rezip ZipList item [] a_items = ZipList item [] a_items
zl_rezip ziplist = zl_rezip <*> zl_up ziplist

zl_append :: ZipList a -> a -> ZipList a
zl_append item ziplist = ZipList item curr_item:prev_items []
    where ZipList curr_item prev_items [] = zl_unzip ziplist
