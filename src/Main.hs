{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import UI.NCurses
import UI.NCurses.Types
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Types
import Projection
import Accounts
import AccountSelector
import TransactionSelector
import SplitSelector
import StatementSelector
import Data.List.Zipper
import Data.Scientific as Scientific
import Control.Lens
import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Char8 (unpack)
import Data.ByteString.Char8 (pack)
import Data.ByteString.Char8 (append)
import Control.Monad.Catch
import Control.Exception (throwIO)
import DrawingStuff
import Data.Maybe
import SQLTypes
import AppTypes
import Apps
import System.Environment

-- TODO: State monad

-- NA  : Bad filters can break program

-- DONE: Confirm quit, rollback, commit

-- DONE: Bulk upload scripts

-- NA  : Overlaid accounts?

-- NA  : virtual accounts

-- NA  : SQL syntax data types?

-- TODO: can FromRow / ToRow use column names, or just position?

-- TODO: get rid of (head res), throw FatalError instead

-- TODO: allow fuzzy account setting

-- DONE: include (uneditable) balance in AccountRow

-- DONE: left box color scrollbar indicator?

-- TODO: Trigger for dates to be contained within year?

-- TODO: Select multiple rows and bulk update

-- TODO: Update starts with current string? Maybe "U" does this?

-- DONE: Make "sticky" scope toggleable; maybe just "peg" scope when desired, never changes when pegged

-- TODO: Is inserting into a statement possible?

-- TODO: Change color to indicate inserting? Hard to do, as window is not being updated

-- TODO: Dynamically select col width, instead of max size

-- TODO: Queries for Becky

-- TODO: Ad hoc interfaces in Apps... is this bad?

-- TODO: Do not draw if not enough horizontal room

-- TODO: Make draw stuff good

-- TODO: ability for app to request new windows?

-- DONE: alternate row colors? periodic spaces?

-- DONE: fix alternate colors toggle problem

-- DONE: Keep place when switching between apps?

-- DONE: Also highlight column name

-- TODO: Clean up main

-- TODO: Remove duplicated code

-- TODO: Key to set status from currently selected field (aid in seeing long fields)

instance MonadThrow Curses where
    throwM e = Curses (throwIO e)

instance MonadCatch Curses where
    catch (Curses a) handler = Curses $ catch a (\e -> unCurses (handler e))

instance Scopeable TransactionRow Int where
    getMaybeScope row _ = Just $ row ^. transactionTid

instance Scopeable AccountRow Int where
    getMaybeScope row _ = Just $ row ^. account_aid

instance Scopeable AccountRow StatementScope where
    getMaybeScope row maybeScope = case maybeScope of
        Just scope -> Just (scope & statementScopeAid .~ (row ^. account_aid))
        Nothing -> Just (StatementScope (row ^. account_aid) (DateKind "1900-01-01") (DateKind "2100-01-01"))

lO1FromAlenses alenses = LO1 (fromList []) (fromList alenses)

accountProj conn = Projection (SimpleSelector conn) (lO1FromAlenses account_alenses)
transactionProj conn = Projection  (SimpleSelector conn) (lO1FromAlenses transactionAlenses)
splitProj conn = Projection (SimpleSelector conn) (lO1FromAlenses splitAlenses)
splitScopedProj conn = Projection (ScopedSelector conn (Nothing::Maybe Int)) (lO1FromAlenses splitScopedAlenses)
transactionSplitProj conn = ProjectionPair (transactionProj conn) (splitScopedProj conn) True 50 False
--statementProjInitialScope = Just (StatementScope 2 (DateKind "2016-01-01") (DateKind "2017-01-01"))
statementProjInitialScope = Nothing :: Maybe StatementScope
cashStatementProjInitialScope = Just (CashScope "%" (DateKind "0100-01-01") (DateKind "3000-01-01"))
cashStatementProj conn = Projection (ScopedSelector conn cashStatementProjInitialScope) (lO1FromAlenses statementAlenses)
statementProj conn = Projection (ScopedSelector conn statementProjInitialScope) (lO1FromAlenses statementAlenses)
accountStatementProj conn = ProjectionPair (accountProj conn) (statementProj conn) True 50 False

-- sql

handleSqlErrorCore core error = do
    case (sqlExecStatus error) of
        NonfatalError -> do
            let newCore = core & coreStatus .~ (unpack (sqlErrorMsg error))
            return newCore
        _ -> do
            let newCore = core & coreStatus .~ (unpack (sqlErrorMsg error))
            let newCore1 = newCore & coreBasicState .~ BasicStateError
            return newCore1

handleSqlErrorState state error = do
    state & coreApp %%~ (\core -> handleSqlErrorCore core error)

specialCheckQuery = Query "SELECT s FROM (SELECT SUM(AMOUNT * CASE WHEN kind = 'credit' THEN -1 ELSE 1 END) AS s FROM splits \
                          \GROUP BY tid) a WHERE s != 0;"

getMainApp state = fromJust (state ^. mainAppList ^? ix (state ^. mainAppIx))

commitState state = do
    let core = state ^. coreApp
    let conn = core ^. coreConnection
    let i = state ^. mainAppIx
    catch (do
                specialCheckRes <- liftIO $ query_ conn specialCheckQuery :: Curses [Only Scientific]
                case specialCheckRes of
                    [] -> do
                            liftIO $ commit conn
                            liftIO $ begin conn
                            let newState1 = state & coreApp . coreBasicState .~ BasicStateNormal
                            let newState2 = newState1 & coreApp . coreStatus .~ "Commit."
                            newState3 <- newState2 & mainAppList . ix i %%~ appSelect
                            return newState3
                    _ -> do
                            let newState1 = state & coreApp . coreBasicState .~ BasicStateNormal
                            let newState2 = newState1 & coreApp . coreStatus .~ "Imbalances will prevent commit from succeeding."
                            return newState2
                )
          (\error -> handleSqlErrorState state error)

stateEraseCurrentWindows state = do
    foldl (>>) (return ()) (fmap (\w -> updateWindow w erase) (fromJust (state ^. mainWindowsList ^? ix (state ^. mainAppIx))))

-- main

main :: IO ()
main = do
    args <- getArgs
    let yearStr = case args of
            (year:args2) -> year
            [] -> "2017"
    conn <- connectPostgreSQL $ "dbname='accounts" `append` (pack yearStr) `append` "' user='matthew' password='matthew'"
    runCurses $ do
        setEcho False
        setCursorMode CursorInvisible
        colorRedID <- newColorID ColorRed ColorDefault 2
        colorYellowID <- newColorID ColorYellow ColorDefault 3
        colorBlueID <- newColorID ColorBlue ColorDefault 4
        colorWhiteID <- newColorID ColorWhite ColorDefault 5
        colorGreenID <- newColorID ColorGreen ColorDefault 6
        let colors = Colors colorRedID colorYellowID colorBlueID colorWhiteID colorGreenID
        let coreApp_ = CoreState conn colors "" BasicStateSelect
        let mainApp0_ = accountStatementProj conn
        let mainApp1_ = transactionSplitProj conn
        let mainApp2_ = cashStatementProj conn
        coreWindows_ <- appInitWindows coreApp_
        mainWindows0_ <- appInitWindows mainApp0_
        mainWindows1_ <- appInitWindows mainApp1_
        mainWindows2_ <- appInitWindows mainApp2_
        let mainAppList_ = [wrapApp mainApp0_, wrapApp mainApp1_, wrapApp mainApp2_]
        let mainWindowsList = [mainWindows0_, mainWindows1_, mainWindows2_]
        let bigState = BigState coreApp_ coreWindows_ mainAppList_ 0 mainWindowsList
        liftIO $ begin conn
        mainLoop bigState

mainLoop :: BigState -> Curses ()
mainLoop state = do
    let core = state ^. coreApp
    let i = state ^. mainAppIx
    case (core ^. coreBasicState) of
        BasicStateSelect -> do
            let newState1 = state & coreApp . coreBasicState .~ BasicStateNormal
            let newState2 = newState1 & coreApp . coreStatus .~ "Select."
            newState3 <- newState2 & mainAppList . ix i %%~ appSelect
            --newState4 <- newState3 & mainAppList . ix 1 %%~ appSelect
            --newState5 <- newState4 & mainAppList . ix 2 %%~ appSelect
            mainLoop newState3
        BasicStateCommit -> do
            newState <- commitState state
            mainLoop newState
        BasicStateRollback -> do
            liftIO $ rollback (core ^. coreConnection)
            liftIO $ begin (core ^. coreConnection)
            let newState1 = state & coreApp . coreBasicState .~ BasicStateNormal
            let newState2 = newState1 & coreApp . coreStatus .~ "Rollback."
            newState3 <- newState2 & mainAppList . ix i %%~ appSelect
            --newState4 <- newState3 & mainAppList . ix 1 %%~ appSelect
            --newState5 <- newState4 & mainAppList . ix 2 %%~ appSelect
            mainLoop newState3
        BasicStateNormal -> do
            appDraw (state ^. coreApp) (state ^. coreWindows) (core ^. coreColors)
            let mainApp = fromJust $ state ^. mainAppList ^? ix i
            let mainWindows = fromJust $ state ^. mainWindowsList ^? ix i
            appDraw mainApp mainWindows (core ^. coreColors)
            render
            event <- catchCurses (getEvent (head $ state ^. coreWindows) Nothing) (\e -> return Nothing)
            case event of
                Just (EventCharacter '1') -> do
                    stateEraseCurrentWindows state
                    let newState = state & coreApp . coreBasicState .~ BasicStateSelect
                    mainLoop (newState & mainAppIx .~ 0)
                Just (EventCharacter '2') -> do
                    stateEraseCurrentWindows state
                    let newState = state & coreApp . coreBasicState .~ BasicStateSelect
                    mainLoop (newState & mainAppIx .~ 1)
                Just (EventCharacter '3') -> do
                    stateEraseCurrentWindows state
                    let newState = state & coreApp . coreBasicState .~ BasicStateSelect
                    mainLoop (newState & mainAppIx .~ 2)
                Nothing -> mainLoop state
                Just event -> do
                    let input = getString (head $ state ^. coreWindows) 0 2
                    maybeNewCoreApp <- appHandleEvent core event input
                    case maybeNewCoreApp of
                        Just newCoreApp -> mainLoop $ state & coreApp .~ newCoreApp
                        Nothing -> do
                            let blankState = state & coreApp . coreStatus .~ ""
                            newState <- catch
                                (do maybeNewMainApp <- appHandleEvent (getMainApp blankState) event input
                                    case maybeNewMainApp of
                                        Just newMainApp -> return (blankState & mainAppList . ix i .~ (wrapApp newMainApp))
                                        Nothing -> return state)
                                (\e -> handleSqlErrorState state e)
                            mainLoop newState
        BasicStateError -> do
            appDraw (state ^. coreApp) (state ^. coreWindows) (core ^. coreColors)
            let mainApp = fromJust $ state ^. mainAppList ^? ix i
            let mainWindows = fromJust $ state ^. mainWindowsList ^? ix i
            appDraw mainApp mainWindows (core ^. coreColors)
            render
            event <- catchCurses (getEvent (head $ state ^. coreWindows) Nothing) (\e -> return Nothing)
            case event of
                Just (EventCharacter 'r') -> mainLoop (state & coreApp . coreBasicState .~ BasicStateRollback)
                _ -> mainLoop state
        BasicStateQuit -> return ()
