{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE UndecidableInstances #-}

module AppTypes where

import UI.NCurses
import Database.PostgreSQL.Simple
import Control.Lens
import DrawingStuff

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
