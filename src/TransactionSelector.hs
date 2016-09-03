{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module TransactionSelector where

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

-- account_kind type

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

-- transaction row type

data TransactionRow = TransactionRow {
    _transactionTid :: Int,
    _transactionDate :: DateKind,
    _transactionDescription :: String
    } deriving (Show)

makeLenses ''TransactionRow

instance FromRow TransactionRow where
   fromRow = TransactionRow <$> field <*> field <*> field

-- IOSelector

instance IOSelector SimpleIOSelector TransactionRow where
    iosSelect selector = query_ (selector ^. selectorConnection) (Query (intercalate "\n" [
        "SELECT tid, date, description",
        "FROM transactions",
        "ORDER BY date",
        ";"
        ]))

    iosInsert selector = do
        let conn = selector ^. selectorConnection
        let queryString = Query (intercalate "\n" [
                "INSERT INTO transactions (date, description)",
                "VALUES (CURRENT_DATE, '')",
                "RETURNING tid, date, description",
                ";"
                ])
        res <- query_ conn queryString
        return (head res)

    iosUpdate selector row = do
        let conn = selector ^. selectorConnection
        let queryString = Query (intercalate "\n" [
                "UPDATE transactions SET date=?, description=?",
                "WHERE tid=? RETURNING tid, date, description",
                ";"
                ])
        res <- query conn queryString (row ^. transactionDate, row ^. transactionDescription, row ^. transactionTid)
        return (head res)

    iosDelete selector row = do
        let conn = selector ^. selectorConnection
        let queryString = "DELETE FROM transactions WHERE tid=?;"
        execute conn queryString (Only (row ^. transactionTid))
        return ()

-- AccLens

transactionAlenses = [transactionTidAlens, transactionDateAlens, transactionDescriptionAlens]

transactionTidAlens = AccLens
    (\row -> show (row ^. transactionTid))
    Nothing
    "tid"

transactionDateAlens = AccLens
    (\row -> (unpackDK $ row ^. transactionDate))
    (Just (\row val -> row & transactionDate .~ (packDK val)))
    "date"

transactionDescriptionAlens = AccLens
    (\row -> (row ^. transactionDescription))
    (Just (\row val -> row & transactionDescription .~ val))
    "description"
