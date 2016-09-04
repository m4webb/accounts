{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import UI.NCurses
import UI.NCurses.Types
import Database.PostgreSQL.Simple
import Projection
import Accounts
import AccountSelector
import TransactionSelector
import SplitSelector
import StatementSelector
import Data.List.Zipper
import Control.Lens
import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Char8 (unpack)
import Control.Monad.Catch
import Control.Exception (throwIO)
import Filter
import DrawingStuff
import Data.Bool
import Queries
import SQLTypes

-- TODO: State monad

-- TODO: Bad filters can break program

-- TODO: Confirm quit, rollback, commit

-- TODO: Child height ratio field in ProjectionPair

-- TODO: Bulk upload scripts

-- TODO: Overlaid accounts?

-- TODO: SQL syntax data types?

-- TODO: can FromRow / ToRow use column names, or just position?

-- TODO: virtual accounts

instance MonadThrow Curses where
    throwM e = Curses (throwIO e)

instance MonadCatch Curses where
    catch (Curses a) handler = Curses $ catch a (\e -> unCurses (handler e))

data BasicState = 
        BasicStateSelect |
        BasicStateCommit |
        BasicStateRollback |
        BasicStateNormal |
        BasicStateError |
        BasicStateQuit
        deriving (Show)

data CoreState = CoreState {
    _coreConnection :: Connection,
    _coreColors :: Colors,
    _coreStatus :: String,
    _coreBasicState :: BasicState
    }

makeLenses ''CoreState

data ProjectionPair ios1 row1 row2 scopeType = ProjectionPair {
    _pairParent :: Projection ios1 row1,
    _pairChild :: Projection (ScopedIOSelector scopeType) row2,
    _pairParentActive :: Bool,
    _pairChildRatio :: Int
    }

makeLenses ''ProjectionPair

instance IDAble TransactionRow where
    getID row = row ^. transactionTid

instance IDAble AccountRow where
    getID row = row ^. account_aid

type CursesInput = String -> Curses (Maybe String)

type Windows = [Window]

class App a where
    appInitWindows :: a -> Curses Windows
    appDraw :: a -> Windows -> Colors -> Curses ()
    appHandleEvent :: a -> Event -> CursesInput -> Curses (Maybe a)
    appSelect :: a -> Curses a

instance App CoreState where
    appInitWindows core = do
        max_y <- fmap (fromInteger . fst) screenSize
        max_x <- fmap (fromInteger . snd) screenSize
        inputWindow <- newWindow 1 (max_x - 1) (max_y - 3) 1
        statusWindow <- newWindow 1 (max_x - 1) (max_y - 2) 1
        return [inputWindow, statusWindow]

    appDraw core (inputWindow:statusWindow:_) colors = do
        updateWindow statusWindow $ do
            clear
            max_x <- fmap (fromInteger . snd) windowSize
            let msg = (show (core ^. coreBasicState)) ++ " " ++ (core ^. coreStatus)
            drawStringPos (clipString (max_x-1) msg) 0 2
        updateWindow (inputWindow) $ do -- this is cheating?
            clear

    appHandleEvent core event input = do
        case event of
            EventCharacter 'r' -> return $ Just (core & coreBasicState .~ BasicStateRollback)
            EventCharacter 'c' -> return $ Just (core & coreBasicState .~ BasicStateCommit)
            EventCharacter 'q' -> return $ Just (core & coreBasicState .~ BasicStateQuit)
            EventCharacter 'b' -> return $ Just (core & coreStatus .~ "")
            _ -> return Nothing

    appSelect core = return core

instance (IOSelector ios row) => App (Projection ios row) where
    appInitWindows proj = do
        max_y <- fmap (fromInteger . fst) screenSize
        max_x <- fmap (fromInteger . snd) screenSize
        mainWindow <- newWindow (max_y - 4) (max_x - 2) 1 1
        return [mainWindow]

    appDraw proj (window:_) colors = drawLO1 window colors (getLO1 proj) True

    appHandleEvent proj event input = do
        maybeNewProj <- i1HandleEvent proj event input
        return maybeNewProj

    appSelect proj = liftIO $ i1_select proj

minimumLO1DrawSize = 8
mLDS = 8

instance (IOSelector ios1 row1, IOSelector (ScopedIOSelector Int) row2, IDAble row1) =>
            App (ProjectionPair ios1 row1 row2 Int) where
    appInitWindows pair = do
        max_y <- fmap (fromInteger . fst) screenSize
        max_x <- fmap (fromInteger . snd) screenSize
        let childSize = min (max mLDS (quot ((max_y - 4) * (toInteger (pair ^. pairChildRatio))) 100)) (max_y - mLDS)
        parentWindow <- newWindow (max_y - 4 - childSize) (max_x - 2) 1 1
        childWindow <- newWindow (childSize) (max_x - 2) (max_y - 4 - childSize) 1
        return [parentWindow, childWindow]

    appDraw pair (parentWindow:childWindow:_) colors = do
        drawLO1 parentWindow colors (getLO1 (pair ^. pairParent)) (pair ^. pairParentActive)
        drawLO1 childWindow colors (getLO1 (pair ^. pairChild)) (not $ pair ^. pairParentActive)

    appHandleEvent pair event input = do
        case event of
            EventCharacter 'w' -> return (Just (pair & pairParentActive %~ not))
            _ -> do
                case (pair ^. pairParentActive) of
                    True -> do
                        maybeNewPairParent <- i1HandleEvent (pair ^. pairParent) event input
                        case maybeNewPairParent of
                            Just newPairParent -> do
                                let newPair1 = pair & pairParent .~ newPairParent
                                newPair2 <- pairSelectChild newPair1
                                return (Just newPair2)
                            Nothing -> return Nothing
                    False -> do
                        maybeNewPairChild <- i1HandleEvent (pair ^. pairChild) event input
                        case maybeNewPairChild of
                            Just newPairChild -> do
                                let newPair = pair & pairChild .~ newPairChild
                                return (Just newPair)
                            Nothing -> return Nothing

    appSelect pair = do
        newPairParent <- liftIO $ i1_select (pair ^. pairParent)
        let newPair1 = pair & pairParent .~ newPairParent
        newPair2 <- pairSelectChild newPair1
        return newPair2

i1HandleEvent a event input = do
    case event of
        EventCharacter 'k' -> liftIO $ fmap Just (i1_up a)
        EventCharacter 'j' -> liftIO $ fmap Just (i1_down a)
        EventCharacter 'h' -> liftIO $ fmap Just (i1_left a)
        EventCharacter 'l' -> liftIO $ fmap Just (i1_right a)
        EventCharacter 's' -> liftIO $ fmap Just (i1_select a)
        EventCharacter 'i' -> liftIO $ fmap Just (i1_insert a)
        EventCharacter 'u' -> do
            maybeUpdateStr <- input "New value?"
            case maybeUpdateStr of
                Just updateStr -> liftIO $ fmap Just (i1_update a updateStr)
                Nothing -> return $ Just a
        EventCharacter 'd' -> do
            maybeUpdateStr <- input "Delete?"
            case maybeUpdateStr of
                Just "yes" -> liftIO $ fmap Just (i1_delete a)
                _ -> return $ Just a
        --EventCharacter 'f' -> do
        --    maybeFilterStr <- input "Filters (CAREFUL!)?"
        --    case maybeFilterStr of
        --        Just filterStr -> liftIO $ i1_set_filters a [Filter filterStr]
        --        Nothing -> return a
        --EventCharacter 'F' -> liftIO $ i1_reset_filters a
        _ -> return Nothing

pairSelectChild pair = do
    let maybeParentRow = safeCursor ((getLO1 (pair ^. pairParent)) ^. lo1_zip_row)
    let scopeId = case maybeParentRow of
            Just row -> getID row
            Nothing -> -1
    let newPairChild1 = pair ^. pairChild & proj_ios . scopedScope .~ scopeId
    newPairChild2 <- liftIO $ i1_select newPairChild1
    let newPair = pair & pairChild .~ newPairChild2
    return newPair

data BigState a = BigState {
    _coreApp :: CoreState,
    _coreWindows :: Windows,
    _mainApp :: a,
    _mainWindows :: Windows
    }

makeLenses ''BigState

lO1FromAlenses alenses = LO1 (fromList []) (fromList alenses) (fromList [])

accountProj conn = Projection (SimpleIOSelector conn) (lO1FromAlenses account_alenses)
transactionProj conn = Projection  (SimpleIOSelector conn) (lO1FromAlenses transactionAlenses)
splitProj conn = Projection (SimpleIOSelector conn) (lO1FromAlenses splitAlenses)
splitScopedProj conn = Projection (ScopedIOSelector conn (3::Int)) (lO1FromAlenses splitScopedAlenses)
transactionSplitProj conn = ProjectionPair (transactionProj conn) (splitScopedProj conn) True 30
statementProj conn = Projection (ScopedIOSelector conn (2::Int)) (lO1FromAlenses statementAlenses)
accountStatementProj conn = ProjectionPair (accountProj conn) (statementProj conn) True 70

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

commitUnsafe core = do
    liftIO $ commit (core ^. coreConnection)
    let newCore1 = core & coreBasicState .~ BasicStateSelect
    let newCore2 = newCore1 & coreStatus .~ "Commit."
    return newCore2

-- main

main :: IO ()
main = do
    conn <- connectPostgreSQL "dbname='accounts' user='matthew' password='matthew'"
    runCurses $ do
        setEcho False
        setCursorMode CursorInvisible
        colorRedID <- newColorID ColorRed ColorDefault 2
        colorYellowID <- newColorID ColorYellow ColorDefault 3
        colorBlueID <- newColorID ColorBlue ColorDefault 4
        colorWhiteID <- newColorID ColorWhite ColorDefault 5
        let colors = Colors colorRedID colorYellowID colorBlueID colorWhiteID
        let coreApp_ = CoreState conn colors "" BasicStateSelect
        let mainApp_ = accountProj conn
        coreWindows_ <- appInitWindows coreApp_
        mainWindows_ <- appInitWindows mainApp_
        let bigState = BigState coreApp_ coreWindows_ mainApp_ mainWindows_
        mainLoop bigState

changeMainApp state newMainApp = do
    foldl (>>) (return ()) (fmap closeWindow (state ^. mainWindows))
    newMainWindows <- appInitWindows newMainApp
    let newCoreApp = (state ^. coreApp) & coreBasicState .~ BasicStateSelect
    mainLoop $ BigState newCoreApp (state ^. coreWindows) newMainApp newMainWindows

mainLoop :: App a => BigState a -> Curses ()
mainLoop state = do
    let core = state ^. coreApp
    case (core ^. coreBasicState) of
        BasicStateSelect -> do
            liftIO $ begin (core ^. coreConnection)
            let newState1 = state & coreApp . coreBasicState .~ BasicStateNormal
            newState2 <- newState1 & mainApp %%~ appSelect
            mainLoop newState2
        BasicStateCommit -> do
            newCoreApp <- catch (commitUnsafe core) (handleSqlErrorCore core)
            mainLoop (state & coreApp .~ newCoreApp)
        BasicStateRollback -> do
            liftIO $ rollback (core ^. coreConnection)
            let newState1= state & coreApp . coreBasicState .~ BasicStateSelect
            let newState2 = newState1 & coreApp . coreStatus .~ "Rollback."
            mainLoop newState2
        BasicStateNormal -> do
            appDraw (state ^. coreApp) (state ^. coreWindows) (core ^. coreColors)
            appDraw (state ^. mainApp) (state ^. mainWindows) (core ^. coreColors)
            render
            event <- getEvent (head $ state ^. coreWindows) Nothing
            case event of
                Just (EventCharacter '1') -> changeMainApp state (accountStatementProj (core ^. coreConnection))
                Just (EventCharacter '2') -> changeMainApp state (transactionSplitProj (core ^. coreConnection))
                Just (EventCharacter '3') -> changeMainApp state (transactionProj (core ^. coreConnection))
                Just (EventCharacter '4') -> changeMainApp state (splitProj (core ^. coreConnection))
                Nothing -> mainLoop state
                Just event -> do
                    let input = getString (head $ state ^. coreWindows) 0 2
                    maybeNewCoreApp <- appHandleEvent core event input
                    case maybeNewCoreApp of
                        Just newCoreApp -> mainLoop $ state & coreApp .~ newCoreApp
                        Nothing -> do
                            let blankState = state & coreApp . coreStatus .~ ""
                            newState <- catch
                                (do maybeNewMainApp <- appHandleEvent (blankState ^. mainApp) event input
                                    case maybeNewMainApp of
                                        Just newMainApp -> return (blankState & mainApp .~ newMainApp)
                                        Nothing -> return state)
                                (\e -> handleSqlErrorState state e)
                            mainLoop newState
        BasicStateError -> do
            appDraw (state ^. coreApp) (state ^. coreWindows) (core ^. coreColors)
            appDraw (state ^. mainApp) (state ^. mainWindows) (core ^. coreColors)
            render
            event <- getEvent (head $ state ^. coreWindows) Nothing
            case event of
                Just (EventCharacter 'r') -> mainLoop (state & coreApp . coreBasicState .~ BasicStateRollback)
                _ -> mainLoop state
        BasicStateQuit -> return ()
