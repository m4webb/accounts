module Accounts where

import Database.PostgreSQL.Simple

class ILO1 a where
    ilo_select :: a -> IO a
    ilo_update :: a -> String -> IO a
    ilo_insert :: a -> IO a
    ilo_delete :: a -> IO a
    ilo_up :: a -> a
    ilo_down :: a -> a
    ilo_left :: a -> a
    ilo_right :: a -> a

data LO1 row = LO1
    row 
    [row]
    [row]
    (IOLens row) 
    [IOLens row]
    [IOLens row]
    deriving (Show)

data IOLens a = IOLens {
    lens_get :: a -> String,
    lens_set :: Maybe (a -> String -> Connection -> IO a),
    lens_name :: String
    }

instance Show (IOLens a) where
    show lens = "IOLens " ++ (lens_name lens)
