{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

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
    _coreMainWindow :: Window,
    _coreStatusWindow :: Window,
    _coreInputWindow :: Window,
    _coreConnection :: Connection,
    _coreColors :: Colors,
    _coreStatus :: String,
    _coreBasicState :: BasicState
    }

makeLenses ''CoreState

type CursesInput = String -> Curses (Maybe String)

class App a where
    appDraw :: a -> Window -> Colors -> Curses ()
    appHandleEvent :: a -> Event -> CursesInput -> Curses (Maybe a)
    appSelect :: a -> Curses a

instance App CoreState where
    appDraw core window colors = do
        updateWindow (core ^. coreStatusWindow) $ do
            clear
            max_x <- fmap (fromInteger . snd) windowSize
            let msg = (show (core ^. coreBasicState)) ++ " " ++ (core ^. coreStatus)
            drawStringPos (clipString (max_x-1) msg) 0 2
        updateWindow (core ^. coreInputWindow) $ do
            clear

    appHandleEvent core event input = do
        case event of
            EventCharacter 'r' -> return $ Just (core & coreBasicState .~ BasicStateRollback)
            EventCharacter 'c' -> return $ Just (core & coreBasicState .~ BasicStateCommit)
            EventCharacter 'q' -> return $ Just (core & coreBasicState .~ BasicStateQuit)
            EventCharacter 'b' -> return $ Just (core & coreStatus .~ "")
            _ -> return Nothing

    appSelect core = return core

instance App (Projection row) where
    appDraw proj window colors = drawLO1 window colors (getLO1 proj)

    appHandleEvent proj event input = do
        newProj <- case event of
            EventCharacter 'k' -> return $ i1_up proj
            EventCharacter 'j' -> return $ i1_down proj
            EventCharacter 'h' -> return $ i1_left proj
            EventCharacter 'l' -> return $ i1_right proj
            EventCharacter 's' -> liftIO $ i1_select proj
            EventCharacter 'i' -> liftIO $ i1_insert proj
            EventCharacter 'u' -> do
                maybeUpdateStr <- input "New value?"
                case maybeUpdateStr of
                    Just updateStr -> liftIO $ i1_update proj updateStr
                    Nothing -> return proj
            EventCharacter 'd' -> do
                maybeUpdateStr <- input "Delete?"
                case maybeUpdateStr of
                    Just "yes" -> liftIO $ i1_delete proj
                    _ -> return proj
            EventCharacter 'f' -> do
                maybeFilterStr <- input "Filters (CAREFUL!)?"
                case maybeFilterStr of
                    Just filterStr -> return $ i1_set_filters proj [Filter filterStr]
                    Nothing -> return proj
            EventCharacter 'F' -> return $ i1_reset_filters proj
            _ -> return proj
        return (Just newProj)

    appSelect proj = liftIO $ i1_select proj

data BigState a = BigState {
    _coreApp :: CoreState,
    _mainApp :: a
    }

makeLenses ''BigState

accountProj conn = Projection (LO1 (fromList []) (fromList account_alenses) (fromList [])) account_selector conn
transactionProj conn = Projection (LO1 (fromList []) (fromList transactionAlenses) (fromList [])) transactionSelector conn
splitProj conn = Projection (LO1 (fromList []) (fromList splitAlenses) (fromList [])) splitSelector conn

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

main :: IO ()
main = do
    conn <- connectPostgreSQL "dbname='accounts' user='matthew' password='matthew'"
    runCurses $ do
        setEcho False
        setCursorMode CursorInvisible
        colorRedID <- newColorID ColorRed ColorDefault 2
        let colors = Colors colorRedID
        max_y <- fmap (fromInteger . fst) screenSize
        max_x <- fmap (fromInteger . snd) screenSize
        mainWindow <- newWindow (max_y - 4) (max_x - 2) 1 1
        inputWindow <- newWindow 1 (max_x - 1) (max_y - 3) 1
        statusWindow <- newWindow 1 (max_x - 1) (max_y - 2) 1
        let coreApp_ = CoreState mainWindow statusWindow inputWindow conn colors "" BasicStateSelect
        let mainApp_ = accountProj conn
        let bigState = BigState coreApp_ mainApp_
        mainLoop bigState

commitUnsafe core = do
    liftIO $ commit (core ^. coreConnection)
    let newCore1 = core & coreBasicState .~ BasicStateSelect
    let newCore2 = newCore1 & coreStatus .~ ""
    return newCore2

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
            appDraw (state ^. coreApp) (core ^. coreMainWindow) (core ^. coreColors)
            appDraw (state ^. mainApp) (core ^. coreMainWindow) (core ^. coreColors)
            render
            event <- getEvent (core ^. coreMainWindow) Nothing
            case event of
                --Just (EventCharacter '1') -> do
                --    let newAppControl = AppControl spaDraw spaProcessEvent spaSelect
                --    let newAppState = SimpleProjectionApp projectionILO1 (accountProj (core ^. coreConnection))
                --    let newCoreState = core & coreBasicState .~ BasicStateRollback
                --    mainLoop $ BigState (state ^. coreControl) newCoreState newAppControl newAppState
                --Just (EventCharacter '2') -> do
                --    let newAppControl = AppControl spaDraw spaProcessEvent spaSelect
                --    let newAppState = SimpleProjectionApp projectionILO1 (transactionProj (core ^. coreConnection))
                --    let newCoreState = core & coreBasicState .~ BasicStateRollback
                --    mainLoop $ BigState (state ^. coreControl) newCoreState newAppControl newAppState
                --Just (EventCharacter '3') -> do
                --    let newAppControl = AppControl spaDraw spaProcessEvent spaSelect
                --    let newAppState = SimpleProjectionApp projectionILO1 (splitProj (core ^. coreConnection))
                --    let newCoreState = core & coreBasicState .~ BasicStateRollback
                --    mainLoop $ BigState (state ^. coreControl) newCoreState newAppControl newAppState
                Nothing -> mainLoop state
                Just event -> do
                    let input = getString (core ^. coreInputWindow) 0 2
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
            appDraw core (core ^. coreMainWindow) (core ^. coreColors)
            appDraw (state ^. mainApp) (core ^. coreMainWindow) (core ^. coreColors)
            render
            event <- getEvent (core ^. coreMainWindow) Nothing
            case event of
                Just (EventCharacter 'r') -> mainLoop (state & coreApp . coreBasicState .~ BasicStateRollback)
                _ -> mainLoop state
        BasicStateQuit -> return ()
