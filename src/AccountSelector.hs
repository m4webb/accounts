{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}

module AccountSelector where

import Accounts
import Data.Maybe
import Data.Bool
--import Data.Scientific as Scientific
import Data.ByteString.Char8 hiding (head)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.Types
import Control.Lens
import Control.Exception
import SQLTypes

-- account row type

data AccountRow = AccountRow {
    _account_aid :: Int,
    _account_kind :: AccountKind,
    _account_name :: String,
    _account_description :: Maybe String,
    _account_balance :: String
    } deriving (Show)

makeLenses ''AccountRow

-- AccLens

account_alenses = [account_aid_alens, account_kind_alens, account_name_alens, account_balance_alens] 
account_alenses_no_aid = [account_kind_alens, account_name_alens, account_balance_alens]

-- account aid alens

account_aid_alens = AccLens
    (\row -> show (row ^. account_aid))
--    Nothing
    False
    "aid"

-- account kind alens

account_kind_get :: AccountRow -> String
account_kind_get row = unpackAK $ row ^. account_kind

--account_kind_set :: AccountRow -> String -> AccountRow
--account_kind_set row val = account_kind .~ (packAK val) $ row

account_kind_alens = AccLens
    account_kind_get
--    (Just account_kind_set)
    True
    "kind"

-- account name alens

account_name_get :: AccountRow -> String
account_name_get row = row ^. account_name

--account_name_set :: AccountRow -> String -> AccountRow
--account_name_set row val = account_name .~ val $ row

account_name_alens = AccLens
    account_name_get
--    (Just account_name_set)
    True
    "name"

-- account description alens 

account_description_get :: AccountRow -> String
account_description_get row = case (row ^. account_description) of
    Nothing -> "None"
    Just val -> val

--account_description_set :: AccountRow -> String -> AccountRow
--account_description_set row "None" = account_description .~ Nothing $ row
--account_description_set row val = account_description .~ (Just val) $ row

account_description_alens = AccLens
    account_description_get
--    (Just account_description_set)
    True
    "description"

account_balance_alens = AccLens
    (\row -> row ^. account_balance)
--    (Just account_name_set)
    False
    "balance"


-- equal when same identity, not same values in general
instance Eq AccountRow where
    row1 == row2 = (row1 ^. account_aid) == (row2 ^. account_aid)

instance FromRow AccountRow where
   fromRow = AccountRow <$> field <*> field <*> field <*> field <*> field

singleAccountQueryFmt = Query (intercalate "\n" [
        "SELECT a.aid, a.kind, a.name, a.description,",
        "TO_CHAR(COALESCE(SUM(s.amount * CASE WHEN s.kind = 'credit' THEN -1 ELSE 1 END), 0), 'MI99990.99')",
        "FROM accounts a LEFT JOIN splits s ON a.aid = s.aid",
        "WHERE a.aid = ?",
        "GROUP BY a.aid, a.kind, a.name, a.description",
        ";"
        ])

instance Selector IO SimpleSelector AccountRow where
    iosSelect selector = query_ (selector ^. selectorConnection) (Query (intercalate "\n" [
        "SELECT a.aid, a.kind, a.name, a.description,",
        "TO_CHAR(COALESCE(SUM(s.amount * CASE WHEN s.kind = 'credit' THEN -1 ELSE 1 END), 0), 'MI99990.99')",
        "FROM accounts a LEFT JOIN splits s ON a.aid = s.aid",
        "GROUP BY a.aid, a.kind, a.name, a.description",
        "ORDER BY kind, name",
        ";"
        ]))

    iosInsert selector = do
        let conn = (selector ^. selectorConnection)
        let query_string = (Query (intercalate "\n" [
                "INSERT INTO accounts (kind, name)",
                "VALUES ('asset', 'account_' || TO_CHAR(CURRVAL('public.accounts_aid_seq'), 'FM0999'))",
                "RETURNING aid",
                ";"
                ]))
        [Only newAid] <- query_ conn query_string :: IO [Only Int]
        [res] <- query conn singleAccountQueryFmt [newAid]
        return res

    iosUpdate selector row lens val = do
        let conn = (selector ^. selectorConnection)
        [Only newAid] <- if 
            | lens == account_kind_alens -> do
                let query_string = (Query (intercalate "\n" [
                        "UPDATE accounts SET kind=?",
                        "WHERE aid=? RETURNING aid",
                        ";"
                        ]))
                query conn query_string (val, row ^. account_aid) :: IO [Only Int]
            | lens == account_name_alens -> do
                let query_string = (Query (intercalate "\n" [
                        "UPDATE accounts SET name=?",
                        "WHERE aid=? RETURNING aid",
                        ";"
                        ]))
                query conn query_string (val, row ^. account_aid) :: IO [Only Int]
            | lens == account_description_alens -> do
                let query_string = (Query (intercalate "\n" [
                        "UPDATE accounts SET description=?",
                        "WHERE aid=? RETURNING aid",
                        ";"
                        ]))
                let insertVal = case val of
                        "None" -> Nothing
                        justVal -> Just justVal
                query conn query_string (insertVal, row ^. account_aid) :: IO [Only Int]
            | otherwise -> throw (SqlError "" NonfatalError (pack "This field cannot be updated.") "" "")
        [res] <- query conn singleAccountQueryFmt [newAid]
        return res

    iosDelete selector row = do
        let conn = (selector ^. selectorConnection)
        let query_string = (Query (intercalate "\n" [
                "DELETE FROM accounts WHERE aid=?",
                ";"
                ]))
        execute conn query_string (Only (row ^. account_aid))
        return ()
