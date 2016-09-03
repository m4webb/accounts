{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

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

-- account_kind type

newtype SplitKind = SplitKind ByteString

instance Show SplitKind where
    show (SplitKind v) = "SplitKind " ++ show v

unpackSK :: SplitKind -> String
unpackSK (SplitKind val) = unpack val

packSK :: String -> SplitKind 
packSK val = SplitKind $ pack val

instance FromField SplitKind where
    fromField f mdata = do
        typ <- typename f
        if typ /= "split_kind"
            then returnError Incompatible f ""
            else case mdata of
                Nothing  -> returnError UnexpectedNull f ""
                Just dat -> return (SplitKind dat)

instance ToField SplitKind where
    toField (SplitKind f) = toField f

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

splitSelector :: IOSelector SplitRow 
splitSelector = IOSelector splitSelect splitInsert splitUpdate splitDelete

splitSelect:: Connection -> [Filter] -> IO [SplitRow]
splitSelect conn filters = query_ conn (Query (intercalate "\n" [
    "SELECT s.sid, s.tid, a.name, s.kind, s.amount, s.memo",
    "FROM splits s",
    "INNER JOIN accounts a on s.aid = a.aid",
    pack (filtersToSql filters),
    "ORDER BY s.tid",
    ";"
    ]))

splitInsertQueryStr = Query (intercalate "\n" [
    "INSERT INTO splits (tid, aid, kind, amount) VALUES (",
    "(SELECT MIN(tid) from transactions),",
    "(SELECT MIN(aid) from accounts),",
    "'debit',",
    "0)",
    "RETURNING sid",
    ";"
    ])

splitInsert :: Connection -> IO SplitRow
splitInsert conn = do
    [Only sid] <- query_ conn splitInsertQueryStr :: IO [Only Int]
    [res] <- splitSelect conn [Filter ("sid=" ++ (show sid))]
    return res

splitUpdateQueryStrFmt = Query (intercalate "\n" [
    "UPDATE splits SET tid=?, aid=?, kind=?, amount=?, memo=?",
    "WHERE sid=? RETURNING sid",
    ";"
    ])

accountAidFromNameQuery = Query "SELECT aid FROM accounts where name=?;"

splitUpdate :: Connection -> SplitRow -> IO SplitRow
splitUpdate conn row = do
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
            [res] <- splitSelect conn [Filter ("sid=" ++ (show sid))]
            return res
        _ -> throw (SqlError "" NonfatalError (pack ("no account named " ++ (row ^. splitAccount))) "" "")

splitDelete :: Connection -> SplitRow -> IO ()
splitDelete conn row = do
    let queryString = "DELETE FROM splits WHERE sid=?;"
    execute conn queryString (Only (row ^. splitSid))
    return ()

-- AccLens

splitAlenses = [splitSidAlens, splitTidAlens, splitAccountAlens, splitKindAlens, splitAmountAlens, splitMemoAlens]

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
