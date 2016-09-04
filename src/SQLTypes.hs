{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module SQLTypes where

import Data.Maybe
import Data.ByteString.Char8 hiding (head)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.Types
import Control.Lens

-- AccountKind

newtype AccountKind = AccountKind ByteString

instance Show AccountKind where
    show (AccountKind v) = "AccountKind " ++ show v

unpackAK :: AccountKind -> String
unpackAK (AccountKind val) = unpack val

packAK :: String -> AccountKind
packAK val = AccountKind $ pack val

instance FromField AccountKind where
    fromField f mdata = do
        typ <- typename f
        if typ /= "account_kind"
            then returnError Incompatible f ""
            else case mdata of
                Nothing  -> returnError UnexpectedNull f ""
                Just dat -> return (AccountKind dat)

instance ToField AccountKind where
    toField (AccountKind f) = toField f

-- DateKind

newtype DateKind = DateKind ByteString

instance Show DateKind where
    show (DateKind v) = "DateKind " ++ show v

unpackDK :: DateKind -> String
unpackDK (DateKind val) = unpack val

packDK :: String -> DateKind 
packDK val = DateKind $ pack val

instance FromField DateKind where
    fromField f mdata = do
        typ <- typename f
        if typ /= "date"
            then returnError Incompatible f ""
            else case mdata of
                Nothing  -> returnError UnexpectedNull f ""
                Just dat -> return (DateKind dat)

instance ToField DateKind where
    toField (DateKind f) = toField f

-- SplitKind

newtype SplitKind = SplitKind ByteString

instance Show SplitKind where
    show (SplitKind v) = "SplitKind " ++ show v

unpackSK :: SplitKind -> String
unpackSK (SplitKind val) = unpack val

packSK :: String -> SplitKind 
packSK val = SplitKind $ pack val

instance FromField SplitKind where
    fromField f mdata = do
        typ <- typename f
        if typ /= "split_kind"
            then returnError Incompatible f ""
            else case mdata of
                Nothing  -> returnError UnexpectedNull f ""
                Just dat -> return (SplitKind dat)

instance ToField SplitKind where
    toField (SplitKind f) = toField f
