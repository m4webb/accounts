{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiWayIf #-}

module StatementSelector where

import Accounts
import Data.Maybe
import Data.List (length)
import Data.ByteString.Char8 hiding (head, length)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.Types
import Control.Lens
import Control.Exception
import Data.Scientific as Scientific
import Filter
import Text.Read
import Queries
import SQLTypes

-- statement row type

data StatementRow = StatementRow {
    _statementSid :: Int,
    _statementDate :: DateKind,
    _statementDescription :: String,
    _statementAccount :: String,
    _statementCounter :: String,
    _statementKind :: SplitKind,
    _statementAmount :: Scientific,
    _statementBalance :: Scientific 
    } deriving (Show)

makeLenses ''StatementRow

instance Eq StatementRow where
    row1 == row2 = (row1 ^. statementSid) == (row2 ^. statementSid)

instance FromRow StatementRow where
   fromRow = StatementRow <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

-- ScopedIOSelector

data StatementScope = StatementScope {
    _statementScopeAid :: Int,
    _statementScopeDateFrom :: DateKind,
    _statementScopeDateTo :: DateKind 
    } deriving (Show)

makeLenses ''StatementScope

instance IOSelector (ScopedIOSelector StatementScope) StatementRow where
    iosSelect scoped = do
        let maybeScope = scoped ^. scopedMaybeScope
        case maybeScope of
            Just scope -> do
                let aid = scope ^. statementScopeAid
                let dateFrom = scope ^. statementScopeDateFrom
                let dateTo = scope ^. statementScopeDateTo
                let conn = scoped ^. scopedConnection
                query conn statementSelectQueryFmt (aid, dateFrom, dateTo)
            Nothing -> return []

    iosInsert scoped = throw (SqlError "" NonfatalError (pack "Cannot insert into statements.") "" "")

    iosUpdate scoped row lens val = do
        let conn = (scoped ^. scopedConnection)
        let sid = row ^. statementSid
        if
            | lens == stmtDateAlens -> do
                let queryFmt = (Query (intercalate "\n" [
                        "UPDATE transactions SET date=?",
                        "WHERE tid IN (SELECT tid FROM splits where sid=?)",
                        ";"
                        ]))
                execute conn queryFmt (val, sid)
            | lens == stmtDescAlens -> do
                let queryFmt = (Query (intercalate "\n" [
                        "UPDATE transactions SET description=?",
                        "WHERE tid IN (SELECT tid FROM splits where sid=?)",
                        ";"
                        ]))
                execute conn queryFmt (val, sid)
            | lens == stmtAccountAlens -> do
                let getAccountAidQueryFmt = Query "SELECT aid FROM accounts WHERE name=?;"
                let updateAccountQueryFmt = (Query (intercalate "\n" [
                        "UPDATE splits SET aid=?",
                        "WHERE sid=?",
                        ";"
                        ]))
                aids <- query conn getAccountAidQueryFmt (Only val) :: IO [Only Int]
                case aids of
                    [Only aid] -> do
                        execute conn updateAccountQueryFmt (aid, sid)
                    _ -> throw (SqlError "" NonfatalError (pack ("no account named " ++ val)) "" "")

            | lens == stmtCounterAlens -> do
                let getAccountAidQueryFmt = Query "SELECT aid FROM accounts WHERE name=?;"
                let checkCounterQueryFmt = (Query (intercalate "\n" [
                        "SELECT s2.sid FROM splits s",
                        "LEFT JOIN transactions t ON s.sid = t.tid",
                        "LEFT JOIN splits s2 ON s.kind != s2.kind AND s.tid = s2.tid",
                        "WHERE s.sid = ?",
                        ";"
                        ]))
                let updateCounterQueryFmt = (Query (intercalate "\n" [
                        "UPDATE splits SET aid=?",
                        "WHERE sid IN (",
                        "SELECT s2.sid FROM splits s",
                        "LEFT JOIN transactions t ON s.sid = t.tid",
                        "LEFT JOIN splits s2 ON s.kind != s2.kind AND s.tid = s2.tid",
                        "WHERE s.sid = ?)",
                        ";"
                        ]))
                counterCheck <- query conn checkCounterQueryFmt (Only sid) :: IO [Only Int]
                case (length counterCheck) of
                    1 -> do
                        counterAids <- query conn getAccountAidQueryFmt (Only val) :: IO [Only Int]
                        case counterAids of
                            [Only counterAid] -> do
                                execute conn updateCounterQueryFmt (counterAid, sid)
                            _ -> throw (SqlError "" NonfatalError (pack ("No account named " ++ (row ^. statementCounter))) "" "")
                    _ -> throw (SqlError "" NonfatalError (pack "Cannot update counter on split transactions.") "" "")
            | otherwise -> throw (SqlError "" NonfatalError (pack "Cannot update field.") "" "")
        res <- query conn statementSelectSingleQueryFmt (Only sid)
        case res of
            [newRow] -> return newRow
            _ -> throw (SqlError "" FatalError (pack "Could not reselect row in statement update.") "" "")
        
    --iosUpdate scoped row = do
    --    let conn = (scoped ^. scopedConnection)
    --    let sid = row ^. statementSid
    --    let getAccountAidQueryFmt = Query "SELECT aid FROM accounts WHERE name=?"
    --    let updateDateDescQueryFmt = (Query (intercalate "\n" [
    --            "UPDATE transactions SET date=?, description=?",
    --            "WHERE tid IN (SELECT tid FROM splits where sid=?)",
    --            ";"
    --            ]))
    --    let updateAccountQueryFmt = (Query (intercalate "\n" [
    --            "UPDATE splits SET aid=?",
    --            "WHERE sid=?",
    --            ";"
    --            ]))
    --    let checkCounterQueryFmt = (Query (intercalate "\n" [
    --            "SELECT s2.sid FROM splits s",
    --            "LEFT JOIN transactions t ON s.sid = t.tid",
    --            "LEFT JOIN splits s2 ON s.kind != s2.kind AND s.tid = s2.tid",
    --            "WHERE s.sid = ?",
    --            ";"
    --            ]))
    --    let updateCounterQueryFmt = (Query (intercalate "\n" [
    --            "UPDATE splits SET aid=?",
    --            "WHERE sid IN (",
    --            "SELECT s2.sid FROM splits s",
    --            "LEFT JOIN transactions t ON s.sid = t.tid",
    --            "LEFT JOIN splits s2 ON s.kind != s2.kind AND s.tid = s2.tid",
    --            "WHERE s.sid = ?)",
    --            ";"
    --            ]))
    --    accountAids <- query conn getAccountAidQueryFmt (row ^. statementAccount) :: IO [Only Int]
    --    case accountAid of
    --        [Only accountAid] -> do
    --            counterCheck <- query conn checkCounterQueryFmt (Only sid)
    --            case (length counterCheck) of
    --                1 -> do
    --                    counterAids <- query conn getAccountAidQueryFmt (row ^. statementCounter) :: IO [Only Int]
    --                    case counterAids of
    --                        [Only counterAid] -> do
    --                            execute conn updateDateDescQueryFmt (row ^. statementDate, row ^. statementDescription, sid)
    --                            execute conn updateAccountQueryFmt (accountAid, sid)
    --                            execute conn updateCounterQueryFmt (counterAid, sid)
    --                            newRow <- query conn statementSelectSingleQueryFmt (sid)
    --                            case newRows of
    --                                [newRow] -> return newRow
    --                                _ -> throw (SqlError "" FatalError (pack "could not select updated statement row") "" "")
    --                        _ ->  throw (SqlError "" NonfatalError (pack ("no account named " ++ (row ^. statementCounter))) "" "")
    --                _ -> do
    --        _ -> throw (SqlError "" NonfatalError (pack ("no account named " ++ (row ^. statementAccount))) "" "")

    iosDelete scoped row = throw (SqlError "" NonfatalError (pack "Cannot delete from statements.") "" "")

-- AccLens

statementAlenses = [
    stmtDateAlens,
    stmtDescAlens,
    stmtAccountAlens,
    stmtCounterAlens,
    stmtKindAlens,
    stmtAmountAlens,
    stmtBalanceAlens
    ]

stmtDateAlens = AccLens
    (\row -> unpackDK (row ^. statementDate))
--    (Just (\row val -> row & statementDate .~ (packDK val)))
    True
    "date"

stmtKindAlens = AccLens
    (\row -> unpackSK (row ^. statementKind))
--    Nothing
    False
    "kind"

stmtAmountAlens = AccLens
    (\row -> show (row ^. statementAmount))
--    Nothing
    False
    "amount"

stmtAccountAlens = AccLens
    (\row -> row ^. statementAccount)
--    (Just (\row val -> row & statementAccount .~ val))
    True
    "account"

stmtCounterAlens = AccLens
    (\row -> row ^. statementCounter)
--    Nothing
    True
    "counter"

stmtDescAlens = AccLens
    (\row -> row ^. statementDescription)
--    (Just (\row val -> row & statementDescription .~ val))
    True
    "description"

stmtBalanceAlens = AccLens
    (\row -> show (row ^. statementBalance))
--    Nothing
    False
    "balance"