{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module SplitSelector where

import Accounts
import Data.Maybe
import Data.ByteString.Char8 hiding (head)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.Types
import Control.Lens
import Control.Exception
import Data.Scientific as Scientific
import Filter
import Text.Read
import SQLTypes

-- transaction row type

data SplitRow = SplitRow {
    _splitSid :: Int,
    _splitTid :: Int,
    _splitAccount :: String,
    _splitKind :: SplitKind,
    _splitAmount :: Scientific,
    _splitMemo :: Maybe String
    } deriving (Show)

makeLenses ''SplitRow

instance FromRow SplitRow where
   fromRow = SplitRow <$> field <*> field <*> field <*> field <*> field <*> field

-- IOSelector

instance IOSelector SimpleIOSelector SplitRow where
    iosSelect selector = query_ (selector ^. selectorConnection) (Query (intercalate "\n" [
        "SELECT s.sid, s.tid, a.name, s.kind, s.amount, s.memo",
        "FROM splits s",
        "INNER JOIN accounts a on s.aid = a.aid",
        "ORDER BY s.tid, s.kind DESC",
        ";"
        ]))

    iosInsert selector = do
        let conn = selector ^. selectorConnection
        let insertQueryStr = Query (intercalate "\n" [
                "INSERT INTO splits (tid, aid, kind, amount) VALUES (",
                "(SELECT MIN(tid) from transactions),",
                "(SELECT MIN(aid) from accounts),",
                "'debit',",
                "0)",
                "RETURNING sid",
                ";"
                ])
        let selectQueryStrFmt = (Query (intercalate "\n" [
                "SELECT s.sid, s.tid, a.name, s.kind, s.amount, s.memo",
                "FROM splits s",
                "INNER JOIN accounts a on s.aid = a.aid",
                "WHERE s.sid=?",
                "ORDER BY s.tid, s.kind DESC",
                ";"
                ]))
        [Only sid] <- query_ conn insertQueryStr :: IO [Only Int]
        [res] <- query conn selectQueryStrFmt [sid]
        return res

    iosUpdate selector row = do
        let conn = selector ^. selectorConnection
        let splitUpdateQueryStrFmt = Query (intercalate "\n" [
                "UPDATE splits SET tid=?, aid=?, kind=?, amount=?, memo=?",
                "WHERE sid=? RETURNING sid",
                ";"
                ])
        let selectQueryStrFmt = (Query (intercalate "\n" [
                "SELECT s.sid, s.tid, a.name, s.kind, s.amount, s.memo",
                "FROM splits s",
                "INNER JOIN accounts a on s.aid = a.aid",
                "WHERE s.sid=?",
                "ORDER BY s.tid, s.kind DESC",
                ";"
                ]))
        let accountAidFromNameQuery = Query "SELECT aid FROM accounts where name=?;"
        aids <- query conn accountAidFromNameQuery [row ^. splitAccount] :: IO [Only Int]
        case aids of
            [Only aid] -> do
                [Only sid] <- query conn splitUpdateQueryStrFmt (
                    row ^. splitTid,
                    aid :: Int,
                    row ^. splitKind,
                    row ^. splitAmount,
                    row ^. splitMemo,
                    row ^.  splitSid
                    ) :: IO [Only Int]
                [res] <- query conn selectQueryStrFmt [sid]
                return res
            _ -> throw (SqlError "" NonfatalError (pack ("no account named " ++ (row ^. splitAccount))) "" "")

    iosDelete selector row = do
        let conn = selector ^. selectorConnection
        let queryString = "DELETE FROM splits WHERE sid=?;"
        execute conn queryString (Only (row ^. splitSid))
        return ()

-- ScopedIOSelector

instance IOSelector (ScopedIOSelector Int) SplitRow where
    iosSelect scoped = do
        let tid = scoped ^. scopedScope
        let conn = scoped ^. scopedConnection
        let selectQueryStrFmt = Query (intercalate "\n" [
                "SELECT s.sid, s.tid, a.name, s.kind, s.amount, s.memo",
                "FROM splits s",
                "INNER JOIN accounts a on s.aid = a.aid",
                "WHERE s.tid=?",
                "ORDER BY s.tid, s.kind DESC",
                ";"
                ])
        query conn selectQueryStrFmt [tid]

    iosInsert scoped = do
        let tid = scoped ^. scopedScope
        let conn = scoped ^. scopedConnection
        let insertQueryStrFmt = Query (intercalate "\n" [
                "INSERT INTO splits (tid, aid, kind, amount)",
                "VALUES (?, (SELECT MIN(aid) from accounts), 'debit', 0)",
                "RETURNING sid",
                ";"
                ])
        let selectQueryStrFmt = (Query (intercalate "\n" [
                "SELECT s.sid, s.tid, a.name, s.kind, s.amount, s.memo",
                "FROM splits s",
                "INNER JOIN accounts a on s.aid = a.aid",
                "WHERE s.sid=?",
                "ORDER BY s.tid, s.kind DESC",
                ";"
                ]))
        [Only sid] <- query conn insertQueryStrFmt [tid] :: IO [Only Int]
        [res] <- query conn selectQueryStrFmt [sid]
        return res

    iosUpdate scoped row = do
        let conn = scoped ^. scopedConnection 
        let splitUpdateQueryStrFmt = Query (intercalate "\n" [
                "UPDATE splits SET tid=?, aid=?, kind=?, amount=?, memo=?",
                "WHERE sid=? RETURNING sid",
                ";"
                ])
        let selectQueryStrFmt = (Query (intercalate "\n" [
                "SELECT s.sid, s.tid, a.name, s.kind, s.amount, s.memo",
                "FROM splits s",
                "INNER JOIN accounts a on s.aid = a.aid",
                "WHERE s.sid=?",
                "ORDER BY s.tid, s.kind DESC",
                ";"
                ]))
        let accountAidFromNameQuery = Query "SELECT aid FROM accounts where name=?;"
        aids <- query conn accountAidFromNameQuery [row ^. splitAccount] :: IO [Only Int]
        case aids of
            [Only aid] -> do
                [Only sid] <- query conn splitUpdateQueryStrFmt (
                    row ^. splitTid,
                    aid :: Int,
                    row ^. splitKind,
                    row ^. splitAmount,
                    row ^. splitMemo,
                    row ^.  splitSid
                    ) :: IO [Only Int]
                [res] <- query conn selectQueryStrFmt [sid]
                return res
            _ -> throw (SqlError "" NonfatalError (pack ("no account named " ++ (row ^. splitAccount))) "" "")

    iosDelete scoped row = do
        let conn = scoped ^. scopedConnection 
        let queryString = "DELETE FROM splits WHERE sid=?;"
        execute conn queryString (Only (row ^. splitSid))
        return ()

-- AccLens

splitAlenses = [splitSidAlens, splitTidAlens, splitAccountAlens, splitKindAlens, splitAmountAlens, splitMemoAlens]

splitScopedAlenses = [splitSidAlens, splitKindAlens, splitAccountAlens, splitAmountAlens, splitMemoAlens]

splitSidAlens = AccLens
    (\row -> show (row ^. splitSid))
    Nothing
    "sid"

splitTidSet row mval = case (readMaybe mval) of
    Nothing -> throw (SqlError "" NonfatalError (pack ("cannot read " ++ mval)) "" "")
    Just val -> row & splitTid .~ val

splitTidAlens = AccLens
    (\row -> show (row ^. splitTid))
    (Just splitTidSet)
    "tid"

splitAccountAlens = AccLens
    (\row -> row ^. splitAccount)
    (Just (\row val -> row & splitAccount .~ val))
    "account"

splitKindGet :: SplitRow -> String
splitKindGet row = unpackSK $ row ^. splitKind

splitKindSet :: SplitRow -> String -> SplitRow
splitKindSet row val = case val of
    "credit" -> row & splitKind .~ (packSK "credit")
    "c" -> row & splitKind .~ (packSK "credit")
    "" -> row & splitKind .~ (packSK "credit")
    "debit" -> row & splitKind .~ (packSK "debit")
    "d" -> row & splitKind .~ (packSK "debit")
    _ ->   throw (SqlError "" NonfatalError (pack (val ++ " must be one of debit, credit")) "" "")

splitKindAlens = AccLens
    splitKindGet
    (Just splitKindSet)
    "kind"

splitAmountSet row mval = case (readMaybe mval) of
    Nothing -> throw (SqlError "" NonfatalError (pack ("cannot read " ++ mval)) "" "")
    Just val -> row & splitAmount .~ val

splitAmountAlens = AccLens
    (\row -> show (row ^. splitAmount))
    (Just splitAmountSet)
    "amount"

splitMemoGet :: SplitRow -> String
splitMemoGet row = case (row ^. splitMemo) of
    Nothing -> "None"
    Just val -> val

splitMemoSet :: SplitRow -> String -> SplitRow
splitMemoSet row "None" = row & splitMemo.~ Nothing
splitMemoSet row val = row & splitMemo .~ (Just val)

splitMemoAlens = AccLens
    splitMemoGet
    (Just splitMemoSet)
    "memo"
