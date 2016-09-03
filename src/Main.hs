{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import UI.NCurses
import UI.NCurses.Types
import Database.PostgreSQL.Simple
import Projection
import Accounts
import AccountSelector
import TransactionSelector
import SplitSelector
import Data.List.Zipper
import Control.Lens
import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Char8 (unpack)
import Control.Monad.Catch
import Control.Exception (throwIO)
import Filter
import DrawingStuff
import Data.Bool

-- TODO: State monad

-- TODO: Bad filters can break program

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

data ProjectionPair ios1 row1 row2 = ProjectionPair {
    _pairParent :: Projection ios1 row1,
    _pairChild :: Projection ScopedIOSelector row2,
    _pairParentActive :: Bool
    }

makeLenses ''ProjectionPair

instance IDAble TransactionRow where
    getID row = row ^. transactionTid

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
        newProj <- i1HandleEvent proj event input
        return (Just newProj)

    appSelect proj = liftIO $ i1_select proj

instance (IOSelector ios1 row1, IOSelector ScopedIOSelector row2, IDAble row1) => App (ProjectionPair ios1 row1 row2) where
    appInitWindows pair = do
        max_y <- fmap (fromInteger . fst) screenSize
        max_x <- fmap (fromInteger . snd) screenSize
        parentWindow <- newWindow (max_y - 4 - 12) (max_x - 2) 1 1
        childWindow <- newWindow (12) (max_x - 2) (max_y - 4 - 12) 1
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
                        newPairParent <- i1HandleEvent (pair ^. pairParent) event input
                        let newPair1 = pair & pairParent .~ newPairParent
                        newPair2 <- pairSelectChild newPair1
                        return (Just newPair2)
                    False -> do
                        newPairChild <- i1HandleEvent (pair ^. pairChild) event input
                        let newPair = pair & pairChild .~ newPairChild
                        return (Just newPair)

    appSelect pair = do
        newPairParent <- liftIO $ i1_select (pair ^. pairParent)
        let newPair1 = pair & pairParent .~ newPairParent
        newPair2 <- pairSelectChild newPair1
        return newPair2


i1HandleEvent a event input = do
    case event of
        EventCharacter 'k' -> liftIO $ i1_up a
        EventCharacter 'j' -> liftIO $ i1_down a
        EventCharacter 'h' -> liftIO $ i1_left a
        EventCharacter 'l' -> liftIO $ i1_right a
        EventCharacter 's' -> liftIO $ i1_select a
        EventCharacter 'i' -> liftIO $ i1_insert a
        EventCharacter 'u' -> do
            maybeUpdateStr <- input "New value?"
            case maybeUpdateStr of
                Just updateStr -> liftIO $ i1_update a updateStr
                Nothing -> return a
        EventCharacter 'd' -> do
            maybeUpdateStr <- input "Delete?"
            case maybeUpdateStr of
                Just "yes" -> liftIO $ i1_delete a
                _ -> return a
        --EventCharacter 'f' -> do
        --    maybeFilterStr <- input "Filters (CAREFUL!)?"
        --    case maybeFilterStr of
        --        Just filterStr -> liftIO $ i1_set_filters a [Filter filterStr]
        --        Nothing -> return a
        --EventCharacter 'F' -> liftIO $ i1_reset_filters a
        _ -> return a

pairSelectChild pair = do
    let maybeParentRow = safeCursor ((getLO1 (pair ^. pairParent)) ^. lo1_zip_row)
    let scopeId = case maybeParentRow of
            Just row -> getID row
            Nothing -> -1
    let newPairChild1 = pair ^. pairChild & proj_ios . scopedId .~ scopeId
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

accountProj conn = Projection (SimpleIOSelector conn) (LO1 (fromList []) (fromList account_alenses) (fromList []))
transactionProj conn = Projection  (SimpleIOSelector conn) (LO1 (fromList []) (fromList transactionAlenses) (fromList []))
splitProj conn = Projection (SimpleIOSelector conn) (LO1 (fromList []) (fromList splitAlenses) (fromList []))
splitScopedProj conn = Projection (ScopedIOSelector conn 3) (LO1 (fromList []) (fromList splitAlenses) (fromList []))
pairProj conn = ProjectionPair (transactionProj conn) (splitScopedProj conn) True

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
    let newCore2 = newCore1 & coreStatus .~ ""
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
        let colors = Colors colorRedID colorYellowID
        let coreApp_ = CoreState conn colors "" BasicStateSelect
        let mainApp_ = accountProj conn
        coreWindows_ <- appInitWindows coreApp_
        mainWindows_ <- appInitWindows mainApp_
        let bigState = BigState coreApp_ coreWindows_ mainApp_ mainWindows_
        mainLoop bigState

changeMainApp state newMainApp = do
    foldl (>>) (return ()) (fmap closeWindow (state ^. mainWindows))
    newMainWindows <- appInitWindows newMainApp
    let newCoreApp = (state ^. coreApp) & coreBasicState .~ BasicStateRollback
    mainLoop $ BigState newCoreApp (state ^. coreWindows) newMainApp newMainWindows

mainLoop :: App a => BigState a -> Curses ()
mainLoop state = do
    let core = state ^. coreApp
    case (core ^. coreBasicState) of
        BasicStateSelect -> do
            liftIO $ begin (core ^. coreConnection)
            let newState1 = state & coreApp . coreStatus .~ ""
            let newState2 = newState1 & coreApp . coreBasicState .~ BasicStateNormal
            newState3 <- newState2 & mainApp %%~ appSelect
            mainLoop newState3
        BasicStateCommit -> do
            newCoreApp <- catch (commitUnsafe core) (handleSqlErrorCore core)
            mainLoop (state & coreApp .~ newCoreApp)
        BasicStateRollback -> do
            liftIO $ rollback (core ^. coreConnection)
            let newState = state & coreApp . coreBasicState .~ BasicStateSelect
            mainLoop newState
        BasicStateNormal -> do
            appDraw (state ^. coreApp) (state ^. coreWindows) (core ^. coreColors)
            appDraw (state ^. mainApp) (state ^. mainWindows) (core ^. coreColors)
            render
            event <- getEvent (head $ state ^. coreWindows) Nothing
            case event of
                Just (EventCharacter '1') -> changeMainApp state (accountProj (core ^. coreConnection))
                Just (EventCharacter '2') -> changeMainApp state (pairProj (core ^. coreConnection))
                Just (EventCharacter '3') -> changeMainApp state (transactionProj (core ^. coreConnection))
                Just (EventCharacter '4') -> changeMainApp state (splitProj (core ^. coreConnection))
                Nothing -> mainLoop state
                Just event -> do
                    let input = getString (head $ state ^. coreWindows) 0 2
                    maybeNewCoreApp <- appHandleEvent core event input
                    case maybeNewCoreApp of
                        Just newCoreApp -> mainLoop $ state & coreApp .~ newCoreApp
                        Nothing -> do
                            newState <- catch
                                (do maybeNewMainApp <- appHandleEvent (state ^. mainApp) event input
                                    case maybeNewMainApp of
                                        Just newMainApp -> return (state & mainApp .~ newMainApp)
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
