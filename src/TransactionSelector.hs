{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}

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
import Control.Exception
import SQLTypes

-- transaction row type

data TransactionRow = TransactionRow {
    _transactionTid :: Int,
    _transactionDate :: DateKind,
    _transactionDescription :: String
    } deriving (Show)

makeLenses ''TransactionRow

instance Eq TransactionRow where
    row1 == row2 = (row1 ^. transactionTid) == (row2 ^. transactionTid)

instance FromRow TransactionRow where
   fromRow = TransactionRow <$> field <*> field <*> field

-- IOSelector

instance IOSelector SimpleIOSelector TransactionRow where
    iosSelect selector = query_ (selector ^. selectorConnection) (Query (intercalate "\n" [
        "SELECT tid, date, description",
        "FROM transactions",
        "ORDER BY date DESC",
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

    iosUpdate selector row lens val = do
        let conn = (selector ^. selectorConnection)
        res <- if
            | lens == transactionDateAlens -> do
                let queryString = (Query (intercalate "\n" [
                        "UPDATE transactions SET date=?",
                        "WHERE tid=? RETURNING tid, date, description",
                        ";"
                        ]))
                query conn queryString (val, row ^. transactionTid)
            | lens == transactionDescriptionAlens -> do
                let queryString = (Query (intercalate "\n" [
                        "UPDATE transactions SET description=?",
                        "WHERE tid=? RETURNING tid, date, description",
                        ";"
                        ]))
                query conn queryString (val, row ^. transactionTid)
            | otherwise -> throw (SqlError "" NonfatalError (pack "This field cannot be updated.") "" "")
        return (head res)

    iosDelete selector row = do
        let conn = selector ^. selectorConnection
        let deleteSplitsQueryFmt = "DELETE FROM splits WHERE tid=?;"
        let deleteTransactionQueryFmt = "DELETE FROM transactions WHERE tid=?;"
        execute conn deleteSplitsQueryFmt (Only (row ^. transactionTid))
        execute conn deleteTransactionQueryFmt (Only (row ^. transactionTid))
        return ()

-- AccLens

transactionAlensesNoId = [transactionDateAlens, transactionDescriptionAlens]

transactionAlenses = [transactionTidAlens, transactionDateAlens, transactionDescriptionAlens]

transactionTidAlens = AccLens
    (\row -> show (row ^. transactionTid))
--    Nothing
    False
    "tid"

transactionDateAlens = AccLens
    (\row -> (unpackDK $ row ^. transactionDate))
--    (Just (\row val -> row & transactionDate .~ (packDK val)))
    True
    "date"

transactionDescriptionAlens = AccLens
    (\row -> (row ^. transactionDescription))
--    (Just (\row val -> row & transactionDescription .~ val))
    True
    "description"
