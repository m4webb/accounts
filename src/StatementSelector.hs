{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module StatementSelector where

import Accounts
import Data.Maybe
import Data.ByteString.Char8 hiding (head)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Control.Lens
import Control.Exception
import Data.Scientific as Scientific
import Filter
import Text.Read
import Queries
import SQLTypes

-- statement row type

data StatementRow = StatementRow {
    _statementDate :: DateKind,
    _statementKind :: SplitKind,
    _statementAmount :: Scientific,
    _statementCounter :: String,
    _statementDescription :: String,
    _statementBalance :: Scientific 
    } deriving (Show)

makeLenses ''StatementRow

instance FromRow StatementRow where
   fromRow = StatementRow <$> field <*> field <*> field <*> field <*> field <*> field

-- ScopedIOSelector

instance IOSelector (ScopedIOSelector Int) StatementRow where
    iosSelect scoped = do
        let tid = scoped ^. scopedScope
        let conn = scoped ^. scopedConnection
        query conn statementSelectQueryFmt [tid]

    iosInsert scoped = throw (SqlError "" NonfatalError (pack "Cannot insert into statements.") "" "")

    iosUpdate scoped row = throw (SqlError "" NonfatalError (pack "Cannot update statements.") "" "")

    iosDelete scoped row = throw (SqlError "" NonfatalError (pack "Cannot delete from statements.") "" "")

-- AccLens

statementAlenses = [stmtDateAlens, stmtKindAlens, stmtAmountAlens, stmtDestAlens, stmtDescAlens, stmtBalanceAlens]

stmtDateAlens = AccLens
    (\row -> unpackDK (row ^. statementDate))
    Nothing
    "date"

stmtKindAlens = AccLens
    (\row -> unpackSK (row ^. statementKind))
    Nothing
    "kind"

stmtAmountAlens = AccLens
    (\row -> show (row ^. statementAmount))
    Nothing
    "amount"

stmtDestAlens = AccLens
    (\row -> row ^. statementCounter)
    Nothing
    "counter"

stmtDescAlens = AccLens
    (\row -> row ^. statementDescription)
    Nothing
    "description"

stmtBalanceAlens = AccLens
    (\row -> show (row ^. statementBalance))
    Nothing
    "balance"
