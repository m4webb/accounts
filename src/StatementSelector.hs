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
--    _statementKind :: SplitKind,
    _statementAmount :: String,
    _statementBalance :: String
    } deriving (Show)

makeLenses ''StatementRow

instance Eq StatementRow where
    row1 == row2 = (row1 ^. statementSid) == (row2 ^. statementSid)

instance FromRow StatementRow where
   fromRow = StatementRow <$> field <*> field <*> field <*> field <*> field <*> field <*> field

-- ScopedSelector

data StatementScope = StatementScope {
    _statementScopeAid :: Int,
    _statementScopeDateFrom :: DateKind,
    _statementScopeDateTo :: DateKind 
    } deriving (Show)

makeLenses ''StatementScope

data CashScope = CashScope {
    _cashScopeDescription :: String,
    _cashScopeDateFrom :: DateKind,
    _cashScopeDateTo :: DateKind
    } deriving (Show)

makeLenses ''CashScope

instance StringSettable CashScope where
    setWithString s (CashScope _ a b) = CashScope s a b

instance Selector IO (ScopedSelector StatementScope) StatementRow where
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
                aids <- query conn getAccountAidQueryFmt [val] :: IO [Only Int]
                case aids of
                    [Only aid] -> do
                        execute conn updateAccountQueryFmt (aid, sid)
                    _ -> throw (SqlError "" NonfatalError (pack ("No account named " ++ val)) "" "")
            | lens == stmtCounterAlens -> do
                let getAccountAidQueryFmt = Query "SELECT aid FROM accounts WHERE name=?;"
                let checkCounterQueryFmt = (Query (intercalate "\n" [
                        "SELECT s2.sid FROM splits s",
                        "INNER JOIN transactions t ON s.tid = t.tid",
                        "INNER JOIN splits s2 ON s.kind != s2.kind AND s.tid = s2.tid",
                        "WHERE s.sid = ?",
                        ";"
                        ]))
                let updateCounterQueryFmt = (Query (intercalate "\n" [
                        "UPDATE splits SET aid=?",
                        "WHERE sid IN (",
                        "SELECT s2.sid FROM splits s",
                        "INNER JOIN transactions t ON s.tid = t.tid",
                        "INNER JOIN splits s2 ON s.kind != s2.kind AND s.tid = s2.tid",
                        "WHERE s.sid = ?)",
                        ";"
                        ]))
                counterCheck <- query conn checkCounterQueryFmt [sid] :: IO [Only Int]
                case (length counterCheck) of
                    1 -> do
                        counterAids <- query conn getAccountAidQueryFmt [val] :: IO [Only Int]
                        case counterAids of
                            [Only counterAid] -> do
                                execute conn updateCounterQueryFmt (counterAid, sid)
                            _ -> throw (SqlError "" NonfatalError (pack ("No account named " ++ val)) "" "")
                    _ -> throw (SqlError "" NonfatalError (pack "Cannot update counter on irregular transactions.") "" "")
            | lens == stmtAmountAlens -> do
                let checkCounterQueryFmt = (Query (intercalate "\n" [
                        "SELECT s2.sid FROM splits s",
                        "INNER JOIN transactions t ON s.tid = t.tid",
                        "INNER JOIN splits s2 ON s.kind != s2.kind AND s.tid = s2.tid",
                        "WHERE s.sid = ?",
                        ";"
                        ]))
                let updateAmountQueryFmt = (Query (intercalate "\n" [
                        "UPDATE splits SET amount=?",
                        "WHERE tid IN (SELECT tid FROM splits WHERE sid=?)",
                        ";"
                        ]))
                counterCheck <- query conn checkCounterQueryFmt [sid] :: IO [Only Int]
                case (length counterCheck) of
                    1 -> do
                        case (readMaybe val :: Maybe Scientific) of
                            Nothing -> throw (SqlError "" NonfatalError (pack ("cannot read " ++ val)) "" "")
                            Just readVal -> do
                                execute conn updateAmountQueryFmt (readVal, sid)
                    _ -> throw (SqlError "" NonfatalError (pack "Cannot update amount on irregular transactions.") "" "")
            | otherwise -> throw (SqlError "" NonfatalError (pack "Cannot update field.") "" "")
        res <- query conn statementSelectSingleQueryFmt [sid]
        case res of
            [newRow] -> return newRow
            _ -> throw (SqlError "" FatalError (pack "Could not reselect row in statement update.") "" "")

    iosDelete scoped row = throw (SqlError "" NonfatalError (pack "Cannot delete from statements.") "" "")

instance Selector IO (ScopedSelector CashScope) StatementRow where
    iosSelect scoped = do
        let maybeScope = scoped ^. scopedMaybeScope
        case maybeScope of
            Just scope -> do
                let description = scope ^. cashScopeDescription
                let dateFrom = scope ^. cashScopeDateFrom
                let dateTo = scope ^. cashScopeDateTo
                let conn = scoped ^. scopedConnection
                query conn statementSelectCashQueryFmt (description, dateFrom, dateTo, description)
            Nothing -> return []

    iosInsert scoped = throw (SqlError "" NonfatalError (pack "Cannot insert into statements.") "" "")

    iosUpdate scoped row lens val = do
        let conn = (scoped ^. scopedConnection)
        let maybeScope = scoped ^. scopedMaybeScope
        let description = case maybeScope of
                Just scope -> scope ^. cashScopeDescription
                Nothing -> "%"
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
                aids <- query conn getAccountAidQueryFmt [val] :: IO [Only Int]
                case aids of
                    [Only aid] -> do
                        execute conn updateAccountQueryFmt (aid, sid)
                    _ -> throw (SqlError "" NonfatalError (pack ("No account named " ++ val)) "" "")
            | lens == stmtCounterAlens -> do
                let getAccountAidQueryFmt = Query "SELECT aid FROM accounts WHERE name=?;"
                let checkCounterQueryFmt = (Query (intercalate "\n" [
                        "SELECT s2.sid FROM splits s",
                        "INNER JOIN transactions t ON s.tid = t.tid",
                        "INNER JOIN splits s2 ON s.kind != s2.kind AND s.tid = s2.tid",
                        "WHERE s.sid = ?",
                        ";"
                        ]))
                let updateCounterQueryFmt = (Query (intercalate "\n" [
                        "UPDATE splits SET aid=?",
                        "WHERE sid IN (",
                        "SELECT s2.sid FROM splits s",
                        "INNER JOIN transactions t ON s.tid = t.tid",
                        "INNER JOIN splits s2 ON s.kind != s2.kind AND s.tid = s2.tid",
                        "WHERE s.sid = ?)",
                        ";"
                        ]))
                counterCheck <- query conn checkCounterQueryFmt [sid] :: IO [Only Int]
                case (length counterCheck) of
                    1 -> do
                        counterAids <- query conn getAccountAidQueryFmt [val] :: IO [Only Int]
                        case counterAids of
                            [Only counterAid] -> do
                                execute conn updateCounterQueryFmt (counterAid, sid)
                            _ -> throw (SqlError "" NonfatalError (pack ("No account named " ++ val)) "" "")
                    _ -> throw (SqlError "" NonfatalError (pack "Cannot update counter on irregular transactions.") "" "")
            | lens == stmtAmountAlens -> do
                let checkCounterQueryFmt = (Query (intercalate "\n" [
                        "SELECT s2.sid FROM splits s",
                        "INNER JOIN transactions t ON s.tid = t.tid",
                        "INNER JOIN splits s2 ON s.kind != s2.kind AND s.tid = s2.tid",
                        "WHERE s.sid = ?",
                        ";"
                        ]))
                let updateAmountQueryFmt = (Query (intercalate "\n" [
                        "UPDATE splits SET amount=?",
                        "WHERE tid IN (SELECT tid FROM splits WHERE sid=?)",
                        ";"
                        ]))
                counterCheck <- query conn checkCounterQueryFmt [sid] :: IO [Only Int]
                case (length counterCheck) of
                    1 -> do
                        case (readMaybe val :: Maybe Scientific) of
                            Nothing -> throw (SqlError "" NonfatalError (pack ("cannot read " ++ val)) "" "")
                            Just readVal -> do
                                execute conn updateAmountQueryFmt (readVal, sid)
                    _ -> throw (SqlError "" NonfatalError (pack "Cannot update amount on irregular transactions.") "" "")
            | otherwise -> throw (SqlError "" NonfatalError (pack "Cannot update field.") "" "")
        res <- query conn statementSelectSingleCashQueryFmt (sid, description)
        case res of
            [newRow] -> return newRow
            _ -> throw (SqlError "" FatalError (pack "Could not reselect row in statement update.") "" "")

    iosDelete scoped row = do
        let conn = scoped ^. scopedConnection
        let sid = row ^. statementSid
        let getTidQuery = "SELECT tid FROM splits WHERE sid=?;"
        let deleteSplitsQuery = "DELETE FROM splits WHERE tid=?;"
        let deleteTransactionQuery = "DELETE FROM transactions WHERE tid=?;"
        [Only tid] <- query conn getTidQuery [sid] :: IO [Only Int]
        execute conn deleteSplitsQuery [tid]
        execute conn deleteTransactionQuery [tid]
        return ()

-- AccLens

statementAlenses = [
    stmtDateAlens,
    stmtDescAlens,
    stmtAccountAlens,
    stmtCounterAlens,
    --stmtKindAlens,
    stmtAmountAlens,
    stmtBalanceAlens
    ]

stmtDateAlens = AccLens
    (\row -> unpackDK (row ^. statementDate))
--    (Just (\row val -> row & statementDate .~ (packDK val)))
    True
    "date"

--stmtKindAlens = AccLens
--    (\row -> unpackSK (row ^. statementKind))
--    Nothing
--    False
--    "kind"

stmtAmountAlens = AccLens
    (\row -> row ^. statementAmount)
--    Nothing
    True
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
    (\row -> row ^. statementBalance)
--    Nothing
    False
    "balance"
