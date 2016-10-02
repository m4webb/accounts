{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE UndecidableInstances #-}

module States where

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
import DrawingStuff
import Data.Bool
import Data.Maybe
import Queries
import SQLTypes

type CursesInput = String -> Curses (Maybe String)

type Windows = [Window]

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

data BigState = BigState {
    _coreApp :: CoreState,
    _coreWindows :: Windows,
    _mainAppList :: [AppWrap],
    _mainAppIx :: Int,
    _mainWindowsList :: [Windows]
    }

makeLenses ''BigState
