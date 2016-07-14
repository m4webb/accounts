{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Accounts where

import Database.PostgreSQL.Simple
import Control.Lens

class I1 a where
    i1_select :: a -> IO a
    --i1_update :: a -> String -> IO a
    --i1_insert :: a -> IO a
    --i1_delete :: a -> IO a
    --i1_up :: a -> a
    --i1_down :: a -> a
    --i1_left :: a -> a
    --i1_right :: a -> a

data LO1 row = LO1 {
    _lo1_zl_row :: (ZipList row),
    _lo1_zl_lens :: (ZipList (IOLens row))
    }

--lo1_set_zl_row lo1 zl_row = lo1 {lo1_zl_row = zl_row}
--lo1_set_zl_lens lo1 zl_lens = lo1 {lo1_zl_lens = zl_lens}

class I1 a => ILO1 a row where
    ilo1_state :: a -> LO1 row

-- lens has field-level scope, selector has row-level scope
    
data IOLens row = IOLens {
    lens_get :: row -> String,
    lens_set :: Maybe (row -> String -> Connection -> IO row),
    lens_name :: String
    }

lens_get_get = lens_get
lens_get_set = lens_set
lens_get_name = lens_name

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

data ZipList a = ZipList {
    zl_item:: a,
    zl_before :: [a],
    zl_after :: [a]
    }

zl_get_item = zl_item
zl_get_before = zl_before
zl_get_after = zl_after
zl_set_item zl item = zl {zl_item = item}
zl_set_before zl before = zl {zl_before = before}
zl_set_after zl after = zl {zl_after = after}

zl_up :: ZipList a -> ZipList a
zl_up (ZipList curr_item (prev_item:before) after) = ZipList prev_item before (curr_item:after)
zl_up (ZipList curr_item [] after) = ZipList curr_item [] after

zl_down :: ZipList a -> ZipList a
zl_down (ZipList curr_item before (next_item:after)) = ZipList next_item (curr_item:before) after
zl_down (ZipList curr_item before []) = ZipList curr_item before []

zl_unzip :: ZipList a -> ZipList a
zl_unzip (ZipList item before []) = ZipList item before []
zl_unzip ziplist = zl_unzip $ zl_down ziplist

zl_rezip :: ZipList a -> ZipList a
zl_rezip (ZipList item [] after) = ZipList item [] after
zl_rezip ziplist = zl_rezip $ zl_up ziplist

zl_append :: ZipList a -> a -> ZipList a
zl_append ziplist new_item = ZipList new_item (item:before) []
    where ZipList item before [] = zl_unzip ziplist

makeLenses ''LO1

