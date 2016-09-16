{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiWayIf #-}

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
    _splitAmount :: String,
    _splitMemo :: Maybe String
    } deriving (Show)

makeLenses ''SplitRow

instance Eq SplitRow where
    row1 == row2 = (row1 ^. splitSid) == (row2 ^. splitSid)

instance FromRow SplitRow where
   fromRow = SplitRow <$> field <*> field <*> field <*> field <*> field <*> field

-- IOSelector

instance IOSelector SimpleIOSelector SplitRow where
    iosSelect selector = query_ (selector ^. selectorConnection) (Query (intercalate "\n" [
        "SELECT s.sid, s.tid, a.name, s.kind, TO_CHAR(s.amount, 'MI99990.00'), s.memo",
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
                "(SELECT aid from accounts WHERE name = 'Imbalance'),",
                "'debit',",
                "0)",
                "RETURNING sid",
                ";"
                ])
        let selectQueryStrFmt = (Query (intercalate "\n" [
                "SELECT s.sid, s.tid, a.name, s.kind, TO_CHAR(s.amount, 'MI99990.00'), s.memo",
                "FROM splits s",
                "INNER JOIN accounts a on s.aid = a.aid",
                "WHERE s.sid=?",
                "ORDER BY s.tid, s.kind DESC",
                ";"
                ]))
        [Only sid] <- query_ conn insertQueryStr :: IO [Only Int]
        [res] <- query conn selectQueryStrFmt [sid]
        return res

    iosUpdate selector row lens val = do
        let conn = (selector ^. selectorConnection)
        let sid = (row ^. splitSid)
        res <- if
            | lens == splitTidAlens -> do
                case (readMaybe val :: Maybe Int) of
                    Nothing -> throw (SqlError "" NonfatalError (pack ("cannot read " ++ val)) "" "")
                    Just readVal -> do
                        let queryFmt = (Query (intercalate "\n" [
                                "UPDATE splits SET tid=?",
                                "WHERE sid=? RETURNING sid",
                                ";"
                                ]))
                        query conn queryFmt (readVal, sid) :: IO [Only Int]
            | lens == splitAccountAlens -> do
                let accountAidFromNameQueryFmt = Query "SELECT aid FROM accounts where name=?;"
                let queryFmt = (Query (intercalate "\n" [
                        "UPDATE splits SET aid=?",
                        "WHERE sid=? RETURNING sid",
                        ";"
                        ]))
                aids <- query conn accountAidFromNameQueryFmt (Only val) :: IO [Only Int]
                case aids of
                    [Only aid] -> do
                        query conn queryFmt (aid, sid)
                    _ -> throw (SqlError "" NonfatalError (pack ("No account named " ++ val)) "" "")
            | lens == splitKindAlens -> do
                let kind = case val of
                        "credit" -> packSK "credit"
                        "c" -> packSK "credit"
                        "" -> packSK "credit"
                        "debit" -> packSK "debit"
                        "d" -> packSK "debit"
                        _ -> throw (SqlError "" NonfatalError (pack (val ++ " must be one of debit, credit")) "" "")
                let queryFmt = (Query (intercalate "\n" [
                        "UPDATE splits SET kind=?",
                        "WHERE sid=? RETURNING sid",
                        ";"
                        ])) 
                query conn queryFmt (kind, sid)
            | lens == splitAmountAlens -> do
               case (readMaybe val :: Maybe Scientific) of
                    Nothing -> throw (SqlError "" NonfatalError (pack ("cannot read " ++ val)) "" "")
                    Just readVal -> do
                        let queryFmt = (Query (intercalate "\n" [
                                "UPDATE splits SET amount=?",
                                "WHERE sid=? RETURNING sid",
                                ";"
                                ]))
                        query conn queryFmt (readVal, sid)
            | lens == splitMemoAlens -> do
                let queryFmt = (Query (intercalate "\n" [
                        "UPDATE splits SET memo=?",
                        "WHERE sid=? RETURNING sid",
                        ";"
                        ])) 
                let insertVal = case val of
                        "None" -> Nothing
                        justVal -> Just justVal
                query conn queryFmt (insertVal, sid)
            | otherwise -> throw (SqlError "" NonfatalError (pack "This field cannot be updated.") "" "")
        case res of
            [Only sid] -> do
                let selectQueryFmt = (Query (intercalate "\n" [
                        "SELECT s.sid, s.tid, a.name, s.kind, TO_CHAR(s.amount, 'MI99990.00'), s.memo",
                        "FROM splits s",
                        "INNER JOIN accounts a on s.aid = a.aid",
                        "WHERE s.sid=?",
                        "ORDER BY s.tid, s.kind DESC",
                        ";"
                        ]))
                res2 <- query conn selectQueryFmt (Only sid)
                case res2 of
                    [newRow] -> return newRow
                    _ -> throw (SqlError "" FatalError (pack "Could not reselect row in split update.") "" "")
            _ -> throw (SqlError "" FatalError (pack "Unexpected query result in split update.") "" "")

    iosDelete selector row = do
        let conn = selector ^. selectorConnection
        let queryString = "DELETE FROM splits WHERE sid=?;"
        execute conn queryString (Only (row ^. splitSid))
        return ()

-- ScopedIOSelector

instance IOSelector (ScopedIOSelector Int) SplitRow where
    iosSelect scoped = do
        let maybeTid = scoped ^. scopedMaybeScope
        case maybeTid of
            Just tid -> do
                let conn = scoped ^. scopedConnection
                let selectQueryStrFmt = Query (intercalate "\n" [
                        "SELECT s.sid, s.tid, a.name, s.kind, TO_CHAR(s.amount, 'MI99990.00'), s.memo",
                        "FROM splits s",
                        "INNER JOIN accounts a on s.aid = a.aid",
                        "WHERE s.tid=?",
                        "ORDER BY s.tid, s.kind DESC",
                        ";"
                        ])
                query conn selectQueryStrFmt [tid]
            Nothing -> return []

    iosInsert scoped = do
        let maybeTid = scoped ^. scopedMaybeScope
        case maybeTid of
            Just tid -> do
                let conn = scoped ^. scopedConnection
                let insertQueryStrFmt = Query (intercalate "\n" [
                        "INSERT INTO splits (tid, aid, kind, amount)",
                        "VALUES (?, (SELECT aid from accounts WHERE name = 'Imbalance'), 'debit', 0)",
                        "RETURNING sid",
                        ";"
                        ])
                let selectQueryStrFmt = (Query (intercalate "\n" [
                        "SELECT s.sid, s.tid, a.name, s.kind, TO_CHAR(s.amount, 'MI99990.00'), s.memo",
                        "FROM splits s",
                        "INNER JOIN accounts a on s.aid = a.aid",
                        "WHERE s.sid=?",
                        "ORDER BY s.tid, s.kind DESC",
                        ";"
                        ]))
                [Only sid] <- query conn insertQueryStrFmt [tid] :: IO [Only Int]
                [res] <- query conn selectQueryStrFmt [sid]
                return res
            Nothing -> throw (SqlError "" NonfatalError (pack "cannot insert with no scope") "" "")

    iosUpdate scoped row lens val = do
        let conn = (scoped ^. scopedConnection)
        let sid = (row ^. splitSid)
        res <- if
            | lens == splitTidAlens -> do
                case (readMaybe val :: Maybe Int) of
                    Nothing -> throw (SqlError "" NonfatalError (pack ("cannot read " ++ val)) "" "")
                    Just readVal -> do
                        let queryFmt = (Query (intercalate "\n" [
                                "UPDATE splits SET tid=?",
                                "WHERE sid=? RETURNING sid",
                                ";"
                                ]))
                        query conn queryFmt (readVal, sid) :: IO [Only Int]
            | lens == splitAccountAlens -> do
                let accountAidFromNameQueryFmt = Query "SELECT aid FROM accounts where name=?;"
                let queryFmt = (Query (intercalate "\n" [
                        "UPDATE splits SET aid=?",
                        "WHERE sid=? RETURNING sid",
                        ";"
                        ]))
                aids <- query conn accountAidFromNameQueryFmt (Only val) :: IO [Only Int]
                case aids of
                    [Only aid] -> do
                        query conn queryFmt (aid, sid)
                    _ -> throw (SqlError "" NonfatalError (pack ("No account named " ++ val)) "" "")
            | lens == splitKindAlens -> do
                let kind = case val of
                        "credit" -> packSK "credit"
                        "c" -> packSK "credit"
                        "" -> packSK "credit"
                        "debit" -> packSK "debit"
                        "d" -> packSK "debit"
                        _ -> throw (SqlError "" NonfatalError (pack (val ++ " must be one of debit, credit")) "" "")
                let queryFmt = (Query (intercalate "\n" [
                        "UPDATE splits SET kind=?",
                        "WHERE sid=? RETURNING sid",
                        ";"
                        ])) 
                query conn queryFmt (kind, sid)
            | lens == splitAmountAlens -> do
               case (readMaybe val :: Maybe Scientific) of
                    Nothing -> throw (SqlError "" NonfatalError (pack ("cannot read " ++ val)) "" "")
                    Just readVal -> do
                        let queryFmt = (Query (intercalate "\n" [
                                "UPDATE splits SET amount=?",
                                "WHERE sid=? RETURNING sid",
                                ";"
                                ]))
                        query conn queryFmt (readVal, sid)
            | lens == splitMemoAlens -> do
                let queryFmt = (Query (intercalate "\n" [
                        "UPDATE splits SET memo=?",
                        "WHERE sid=? RETURNING sid",
                        ";"
                        ])) 
                let insertVal = case val of
                        "None" -> Nothing
                        justVal -> Just justVal
                query conn queryFmt (insertVal, sid)
            | otherwise -> throw (SqlError "" NonfatalError (pack "This field cannot be updated.") "" "")
        case res of
            [Only sid] -> do
                let selectQueryFmt = (Query (intercalate "\n" [
                        "SELECT s.sid, s.tid, a.name, s.kind, TO_CHAR(s.amount, 'MI99990.00'), s.memo",
                        "FROM splits s",
                        "INNER JOIN accounts a on s.aid = a.aid",
                        "WHERE s.sid=?",
                        "ORDER BY s.tid, s.kind DESC",
                        ";"
                        ]))
                res2 <- query conn selectQueryFmt (Only sid)
                case res2 of
                    [newRow] -> return newRow
                    _ -> throw (SqlError "" FatalError (pack "Could not reselect row in split update.") "" "")
            _ -> throw (SqlError "" FatalError (pack "Unexpected query result in split update.") "" "")

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
--    Nothing
    False
    "sid"

--splitTidSet row mval = case (readMaybe mval) of
--    Nothing -> throw (SqlError "" NonfatalError (pack ("cannot read " ++ mval)) "" "")
--    Just val -> row & splitTid .~ val

splitTidAlens = AccLens
    (\row -> show (row ^. splitTid))
--    (Just splitTidSet)
    True
    "tid"

splitAccountAlens = AccLens
    (\row -> row ^. splitAccount)
--    (Just (\row val -> row & splitAccount .~ val))
    True
    "account"

splitKindGet :: SplitRow -> String
splitKindGet row = unpackSK $ row ^. splitKind

--splitKindSet :: SplitRow -> String -> SplitRow
--splitKindSet row val = case val of
--    "credit" -> row & splitKind .~ (packSK "credit")
--    "c" -> row & splitKind .~ (packSK "credit")
--    "" -> row & splitKind .~ (packSK "credit")
--    "debit" -> row & splitKind .~ (packSK "debit")
--    "d" -> row & splitKind .~ (packSK "debit")
--    _ ->   throw (SqlError "" NonfatalError (pack (val ++ " must be one of debit, credit")) "" "")

splitKindAlens = AccLens
    splitKindGet
--    (Just splitKindSet)
    True
    "kind"

--splitAmountSet row mval = case (readMaybe mval) of
--    Nothing -> throw (SqlError "" NonfatalError (pack ("cannot read " ++ mval)) "" "")
--    Just val -> row & splitAmount .~ val

splitAmountAlens = AccLens
    (\row -> row ^. splitAmount)
--    (Just splitAmountSet)
    True
    "amount"

splitMemoGet :: SplitRow -> String
splitMemoGet row = case (row ^. splitMemo) of
    Nothing -> "None"
    Just val -> val

--splitMemoSet :: SplitRow -> String -> SplitRow
--splitMemoSet row "None" = row & splitMemo.~ Nothing
--splitMemoSet row val = row & splitMemo .~ (Just val)

splitMemoAlens = AccLens
    splitMemoGet
--    (Just splitMemoSet)
    True
    "memo"
