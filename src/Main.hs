{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExistentialQuantification #-}

module Main where

import UI.NCurses
import UI.NCurses.Types
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Transaction
import Database.PostgreSQL.Simple.Types
import Projection
import Accounts
import AccountSelector
import TransactionSelector
import SplitSelector
import StatementSelector
import Data.List.Zipper
import Control.Lens.Tuple
import Data.Scientific as Scientific
import Control.Lens
import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Char8 (unpack)
import Control.Monad.Catch
import Control.Exception (throwIO)
import Filter
import DrawingStuff
import Data.Bool
import Data.Maybe
import Queries
import SQLTypes

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

-- TODO: include (uneditable) balance in AccountRow

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

data ProjectionPair ios1 row1 ios2 row2 = ProjectionPair {
    _pairParent :: Projection ios1 row1,
    _pairChild :: Projection ios2 row2,
    _pairParentActive :: Bool,
    _pairChildRatio :: Int,
    _pairChildLocked :: Bool
    }

makeLenses ''ProjectionPair

-- LO1 draw contexts

--LO1DrawContext = LO1DrawContext {
--    lo1dcNormalColor :: Colors -> ColorID,
--    lo1dcGridColor :: Colors -> ColorID,
--    lo1dcCanSetColor :: Colors -> ColorID,
--    lo1dcCannotSetColor :: Colors -> ColorID
--    }

normalLO1Context = LO1DrawContext (view colorWhite) (view colorWhite) (view colorGreen) (view colorYellow)
activeLO1Context = LO1DrawContext (view colorWhite) (view colorGreen) (view colorGreen) (view colorYellow)
inactiveLO1Context = LO1DrawContext (view colorWhite) (view colorYellow) (view colorGreen) (view colorYellow)
activeLockedLO1Context = LO1DrawContext (view colorWhite) (view colorGreen) (view colorGreen) (view colorYellow)
inactiveLockedLO1Context = LO1DrawContext (view colorWhite) (view colorRed) (view colorGreen) (view colorYellow)

instance Scopeable TransactionRow Int where
    getMaybeScope row maybeScope = Just $ row ^. transactionTid

instance Scopeable AccountRow Int where
    getMaybeScope row maybeScope = Just $ row ^. account_aid

instance Scopeable AccountRow StatementScope where
    getMaybeScope row maybeScope = case maybeScope of
        Just scope -> Just (scope & statementScopeAid .~ (row ^. account_aid))
        Nothing -> Just (StatementScope (row ^. account_aid) (DateKind "1900-01-01") (DateKind "2100-01-01"))

type CursesInput = String -> Curses (Maybe String)

type Windows = [Window]

confirmCommand input command alternative = do
    maybeUpdateStr <- input
    case maybeUpdateStr of
        Just "yes" -> command
        _ -> alternative

class App a where
    appInitWindows :: a -> Curses Windows
    appDraw :: a -> Windows -> Colors -> Curses ()
    appHandleEvent :: a -> Event -> CursesInput -> Curses (Maybe a)
    appSelect :: a -> Curses a

data AppWrap = forall a. (App a) => AppWrap a

wrapApp :: App a => a -> AppWrap
wrapApp = AppWrap

instance App AppWrap where
    appInitWindows (AppWrap a) = appInitWindows a

    appDraw (AppWrap a) = appDraw a

    appHandleEvent (AppWrap a) event input = do
        maybeNewApp <- appHandleEvent a event input
        case maybeNewApp of
            Just newApp -> return (Just (AppWrap newApp))
            Nothing -> return Nothing

    appSelect (AppWrap a) = do
        newApp <- appSelect a
        return (AppWrap newApp)

instance App CoreState where
    appInitWindows core = do
        max_y <- fmap (fromInteger . fst) screenSize
        max_x <- fmap (fromInteger . snd) screenSize
        inputWindow <- newWindow 1 (max_x - 1) (max_y - 3) 1
        statusWindow <- newWindow 1 (max_x - 1) (max_y - 2) 1
        return [inputWindow, statusWindow]

    appDraw core (inputWindow:statusWindow:_) colors = do
        updateWindow statusWindow $ do
            erase
            max_x <- fmap (fromInteger . snd) windowSize
            --let msg = (show (core ^. coreBasicState)) ++ " " ++ (core ^. coreStatus)
            let msg = core ^. coreStatus
            drawStringPos (clipString (max_x-1) msg) 0 2
        updateWindow inputWindow $ do -- this is cheating?
            erase
            return ()

    appHandleEvent core event input = do
        case event of
            EventCharacter 's' -> confirmCommand (input "Select?")
                    (return (Just $ core & coreBasicState .~ BasicStateSelect)) (return (Just core))
            EventCharacter 'r' -> confirmCommand (input "Rollback?")
                    (return (Just $ core & coreBasicState .~ BasicStateRollback)) (return (Just core))
            EventCharacter 'c' -> confirmCommand (input "Commit?")
                    (return (Just $ core & coreBasicState .~ BasicStateCommit)) (return (Just core))
            EventCharacter 'q' -> confirmCommand (input "Quit?")
                    (return (Just $ core & coreBasicState .~ BasicStateQuit)) (return (Just core))
            EventCharacter 'b' -> return (Just $ (core & coreStatus .~ ""))
            _ -> return Nothing

    appSelect core = return core

--instance (IOSelector ios row, Eq row) => App (Projection ios row) where
--    appInitWindows proj = do
--        max_y <- fmap (fromInteger . fst) screenSize
--        max_x <- fmap (fromInteger . snd) screenSize
--        mainWindow <- newWindow (max_y - 4) (max_x - 2) 1 1
--        return [mainWindow]
--
--    appDraw proj (window:_) colors = drawLO1 window colors normalLO1Context (getLO1 proj)
--
--    appHandleEvent proj event input = do
--        maybeNewProj <- i1HandleEvent proj event input
--        return maybeNewProj
--
--    appSelect proj = liftIO $ i1_select proj

instance StringSettable CashScope where
    setWithString s (CashScope _ a b) = CashScope s a b

instance (IOSelector (ScopedIOSelector scopeType) row, Eq row, StringSettable scopeType) =>
        App (Projection (ScopedIOSelector scopeType) row) where
    appInitWindows proj = do
        max_y <- fmap (fromInteger . fst) screenSize
        max_x <- fmap (fromInteger . snd) screenSize
        mainWindow <- newWindow (max_y - 4) (max_x - 2) 1 1
        return [mainWindow]

    appDraw proj (window:_) colors = drawLO1 window colors normalLO1Context (getLO1 proj)

    appHandleEvent proj event input = do
         case event of
            EventCharacter 'z' -> do
                let maybeScope = proj ^. proj_ios ^. scopedMaybeScope
                case maybeScope of
                    Just scope -> do
                        maybeScopeInput <- input "Set scope?"
                        case maybeScopeInput of
                            Just scopeInput -> do
                                let newScope = setWithString scopeInput scope
                                let newProj = proj & proj_ios . scopedMaybeScope .~ Just newScope
                                newProj2 <- appSelect newProj
                                return $ Just (newProj2)
                            Nothing -> return (Just proj)
                    Nothing -> return (Just proj)
            _ -> do
                maybeNewProj <- i1HandleEvent proj event input
                return maybeNewProj

    appSelect proj = liftIO $ i1_select proj

minimumLO1DrawSize = 8
mLDS = 8

incRatio x = (min 100 (x+5))
decRatio x = (max 0 (x-5))

instance (IOSelector ios1 row1, IOSelector (ScopedIOSelector scopeType) row2, Scopeable row1 scopeType, Eq row1, Eq row2) =>
            App (ProjectionPair ios1 row1 (ScopedIOSelector scopeType) row2) where
    appInitWindows pair = do
        max_y <- fmap (fromInteger . fst) screenSize
        max_x <- fmap (fromInteger . snd) screenSize
        let childSize = min (max mLDS (quot ((max_y - 4) * (toInteger (pair ^. pairChildRatio))) 100)) (max_y - mLDS)
        parentWindow <- newWindow (max_y - 4 - childSize) (max_x - 2) 1 1
        childWindow <- newWindow (childSize) (max_x - 2) (max_y - 4 - childSize) 1
        return [parentWindow, childWindow]

    appDraw pair (parentWindow:childWindow:_) colors = do
        case (pair ^. pairParentActive) of
            True -> do
                drawLO1 parentWindow colors activeLO1Context (getLO1 (pair ^. pairParent))
                case (pair ^. pairChildLocked) of
                    True -> drawLO1 childWindow colors inactiveLockedLO1Context (getLO1 (pair ^. pairChild))
                    False -> drawLO1 childWindow colors inactiveLO1Context (getLO1 (pair ^. pairChild))
            False -> do
                drawLO1 parentWindow colors inactiveLO1Context (getLO1 (pair ^. pairParent))
                case (pair ^. pairChildLocked) of
                    True -> drawLO1 childWindow colors activeLockedLO1Context (getLO1 (pair ^. pairChild))
                    False -> drawLO1 childWindow colors activeLO1Context (getLO1 (pair ^. pairChild))

    appHandleEvent pair event input = do
        case event of
            EventCharacter 'w' -> return (Just (pair & pairParentActive %~ not))
            EventCharacter 'P' -> return (Just (pair & pairChildLocked %~ not))
            EventCharacter 'p' -> fmap Just (pairChangeChildScope pair)
            _ -> do
                case (pair ^. pairParentActive) of
                    True -> do
                        maybeNewPairParent <- i1HandleEvent (pair ^. pairParent) event input
                        case maybeNewPairParent of
                            Just newPairParent -> do
                                let newPair1 = pair & pairParent .~ newPairParent
                                case pair ^. pairChildLocked of
                                    True -> return (Just newPair1)
                                    False -> fmap Just (pairChangeChildScope newPair1)
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
        let currentMaybeScope = newPair1 ^. pairChild ^. proj_ios ^. scopedMaybeScope
        case currentMaybeScope of
                Nothing -> pairChangeChildScope newPair1
                Just _ -> do
                    newPairChild <- liftIO $ i1_select (newPair1 ^. pairChild)
                    let newPair2 = newPair1 & pairChild .~ newPairChild
                    return newPair2

i1HandleEvent a event input = do
    case event of
        EventCharacter 'k' -> liftIO $ fmap Just (i1_up a)
        EventCharacter 'K' -> liftIO $ fmap Just (i1_up_alot a)
        EventCharacter 'j' -> liftIO $ fmap Just (i1_down a)
        EventCharacter 'J' -> liftIO $ fmap Just (i1_down_alot a)
        EventCharacter 'h' -> liftIO $ fmap Just (i1_left a)
        EventCharacter 'l' -> liftIO $ fmap Just (i1_right a)
        EventCharacter 'g' -> liftIO $ fmap Just (i1_start a)
        EventCharacter 'G' -> liftIO $ fmap Just (i1_end a)
        --EventCharacter 's' -> liftIO $ fmap Just (i1_select a)
        EventCharacter 'i' -> confirmCommand (input "Insert?") (liftIO $ fmap Just (i1_insert a)) (return (Just a))
        EventCharacter 'u' -> do
            maybeUpdateStr <- input "New value?"
            case maybeUpdateStr of
                Just updateStr -> liftIO $ fmap Just (i1_update a updateStr)
                Nothing -> return $ Just a
        EventCharacter 'd' -> confirmCommand (input "Delete?") (fmap Just (liftIO $ i1_delete a)) (return (Just a))
        --EventCharacter 'f' -> do
        --    maybeFilterStr <- input "Filters (CAREFUL!)?"
        --    case maybeFilterStr of
        --        Just filterStr -> liftIO $ i1_set_filters a [Filter filterStr]
        --        Nothing -> return a
        --EventCharacter 'F' -> liftIO $ i1_reset_filters a
        _ -> return Nothing

pairChangeChildScope pair = do
    let currentMaybeScope = pair ^. pairChild ^. proj_ios ^. scopedMaybeScope
    let maybeParentRow = safeCursor ((getLO1 (pair ^. pairParent)) ^. lo1_zip_row)
    let newMaybeScope = case maybeParentRow of
            Just row -> getMaybeScope row currentMaybeScope
            Nothing -> Nothing
    let newPairChild1 = pair ^. pairChild & proj_ios . scopedMaybeScope .~ newMaybeScope
    newPairChild2 <- liftIO $ i1_select newPairChild1
    let newPair = pair & pairChild .~ newPairChild2
    return newPair

data BigState = BigState {
    _coreApp :: CoreState,
    _coreWindows :: Windows,
    _mainAppList :: [AppWrap],
    _mainAppIx :: Int,
    _mainWindowsList :: [Windows]
    }

makeLenses ''BigState

lO1FromAlenses alenses = LO1 (fromList []) (fromList alenses) (fromList [])

accountProj conn = Projection (SimpleIOSelector conn) (lO1FromAlenses account_alenses)
transactionProj conn = Projection  (SimpleIOSelector conn) (lO1FromAlenses transactionAlenses)
splitProj conn = Projection (SimpleIOSelector conn) (lO1FromAlenses splitAlenses)
splitScopedProj conn = Projection (ScopedIOSelector conn (Nothing::Maybe Int)) (lO1FromAlenses splitScopedAlenses)
transactionSplitProj conn = ProjectionPair (transactionProj conn) (splitScopedProj conn) True 50 False
--statementProjInitialScope = Just (StatementScope 2 (DateKind "2016-01-01") (DateKind "2017-01-01"))
statementProjInitialScope = Nothing :: Maybe StatementScope
cashStatementProjInitialScope = Just (CashScope "%" (DateKind "2016-01-01") (DateKind "2017-01-01"))
cashStatementProj conn = Projection (ScopedIOSelector conn cashStatementProjInitialScope) (lO1FromAlenses statementAlenses)
statementProj conn = Projection (ScopedIOSelector conn statementProjInitialScope) (lO1FromAlenses statementAlenses)
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

stateEraseCurrentWindows state = do
    foldl (>>) (return ()) (fmap (\w -> updateWindow w erase) (fromJust (state ^. mainWindowsList ^? ix (state ^. mainAppIx))))

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
            event <- getEvent (head $ state ^. coreWindows) Nothing
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
            event <- getEvent (head $ state ^. coreWindows) Nothing
            case event of
                Just (EventCharacter 'r') -> mainLoop (state & coreApp . coreBasicState .~ BasicStateRollback)
                _ -> mainLoop state
        BasicStateQuit -> return ()
