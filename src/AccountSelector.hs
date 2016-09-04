{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module AccountSelector where

import Accounts
import Filter
import Data.Maybe
import Data.ByteString.Char8 hiding (head)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.Types
import Control.Lens
import SQLTypes

-- account row type

data AccountRow = AccountRow {
    _account_aid :: Int,
    _account_kind :: AccountKind,
    _account_name :: String,
    _account_description :: Maybe String
    } deriving (Show)

makeLenses ''AccountRow

instance FromRow AccountRow where
   fromRow = AccountRow <$> field <*> field <*> field <*> field

instance IOSelector SimpleIOSelector AccountRow where
    iosSelect selector = query_ (selector ^. selectorConnection) (Query (intercalate "\n" [
        "SELECT aid, kind, name, description",
        "FROM accounts",
        "ORDER BY name",
        ";"
        ]))

    iosInsert selector = do
        let conn = (selector ^. selectorConnection)
        let query_string = (Query (intercalate "\n" [
                "INSERT INTO accounts (kind, name)",
                "VALUES ('asset', 'account_' || TO_CHAR(CURRVAL('public.accounts_aid_seq'), 'FM0999'))",
                "RETURNING aid, kind, name, description",
                ";"
                ]))
        res <- query_ conn query_string
        return (head res)

    iosUpdate selector row = do
        let conn = (selector ^. selectorConnection)
        let query_string = (Query (intercalate "\n" [
                "UPDATE accounts SET kind=?, name=?, description=?",
                "WHERE aid=? RETURNING aid, kind, name, description",
                ";"
                ]))
        res <- query conn query_string (row ^. account_kind, row ^. account_name, row ^. account_description, row ^. account_aid)
        return (head res)

    iosDelete selector row = do
        let conn = (selector ^. selectorConnection)
        let query_string = (Query (intercalate "\n" [
                "DELETE FROM accounts WHERE aid=?",
                ";"
                ]))
        execute conn query_string (Only (row ^. account_aid))
        return ()

-- AccLens

account_alenses = [account_aid_alens, account_kind_alens, account_name_alens, account_description_alens]
account_alenses_no_aid = [account_kind_alens, account_name_alens, account_description_alens]

-- account aid alens

account_aid_alens = AccLens
    (\row -> show (row ^. account_aid))
    Nothing
    "aid"

-- account kind alens

account_kind_get :: AccountRow -> String
account_kind_get row = unpackAK $ row ^. account_kind

account_kind_set :: AccountRow -> String -> AccountRow
account_kind_set row val = account_kind .~ (packAK val) $ row

account_kind_alens = AccLens
    account_kind_get
    (Just account_kind_set)
    "kind"

-- account name alens

account_name_get :: AccountRow -> String
account_name_get row = row ^. account_name

account_name_set :: AccountRow -> String -> AccountRow
account_name_set row val = account_name .~ val $ row

account_name_alens = AccLens
    account_name_get
    (Just account_name_set)
    "name"

-- account description alens 

account_description_get :: AccountRow -> String
account_description_get row = case (row ^. account_description) of
    Nothing -> "None"
    Just val -> val

account_description_set :: AccountRow -> String -> AccountRow
account_description_set row "None" = account_description .~ Nothing $ row
account_description_set row val = account_description .~ (Just val) $ row

account_description_alens = AccLens
    account_description_get
    (Just account_description_set)
    "description"
