{-# LANGUAGE OverloadedStrings #-}

module AccountsLenses where

import Accounts
import Data.Maybe
import Data.ByteString hiding (head)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.FromField

-- account_kind data type

newtype AccountKind = AccountKind ByteString deriving (Show)

instance FromField AccountKind where
   fromField f mdata = do
      typ <- typename f
      if typ /= "account_kind"
        then returnError Incompatible f ""
        else case mdata of
               Nothing  -> returnError UnexpectedNull f ""
               Just dat -> return (AccountKind dat)

-- account

data AccountRow = AccountRow {
    _account_aid :: Int,
    _account_kind :: AccountKind,
    _account_name :: String,
    _account_description :: Maybe String
    }
    deriving (Show)

instance FromRow AccountRow where
   fromRow = AccountRow <$> field <*> field <*> field <*> field

account_lenses = [account_aid, account_kind, account_name, account_description]

-- account aid

account_aid = IOLens
    (\row -> show (_account_aid row))
    Nothing
    "aid"

-- account kind 

account_kind_set :: AccountRow -> String -> Connection -> IO AccountRow
account_kind_set row val conn = do
    let update_query = "UPDATE accounts SET kind = ? WHERE aid = ? RETURNING aid, kind, name, description;"
    new_row <- (query conn update_query (val, _account_aid row))
    return (head new_row)

account_kind = IOLens
    (\row -> show (_account_kind row))
    (Just account_kind_set)
    "kind"

-- account name 

account_name_set :: AccountRow -> String -> Connection -> IO AccountRow
account_name_set row val conn = do
    let update_query = "UPDATE accounts SET name = ? WHERE aid = ? RETURNING aid, kind, name, description;"
    new_row <- (query conn update_query (val, _account_aid row))
    return (head new_row)

account_name = IOLens
    _account_name
    (Just account_name_set)
    "name"

-- account description 

account_description_set :: AccountRow -> String -> Connection -> IO AccountRow
account_description_set row val conn = do
    let update_query = "UPDATE accounts SET description = ? WHERE aid = ? RETURNING aid, kind, name, description;"
    new_row <- (query conn update_query (val, _account_aid row))
    return (head new_row)

account_description_get :: AccountRow -> String
account_description_get (AccountRow _ _ _ (Just d)) = d
account_description_get (AccountRow _ _ _ Nothing) = "<nothing>"

account_description = IOLens
    account_description_get
    (Just account_description_set)
    "description"
