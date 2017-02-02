{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE UndecidableInstances #-}

module Apps where

import UI.NCurses
import Projection
import Accounts
import Data.List.Zipper
import Control.Lens
import Control.Monad.IO.Class (liftIO)
import DrawingStuff
import AppTypes

minimumLO1DrawSize = 8
mLDS = 8

incRatio x = (min 100 (x+5))
decRatio x = (max 0 (x-5))

normalLO1Context = LO1DrawContext (view colorWhite) (view colorWhite) (view colorGreen) (view colorYellow)
activeLO1Context = LO1DrawContext (view colorWhite) (view colorGreen) (view colorGreen) (view colorYellow)
inactiveLO1Context = LO1DrawContext (view colorWhite) (view colorYellow) (view colorGreen) (view colorYellow)
activeLockedLO1Context = LO1DrawContext (view colorWhite) (view colorGreen) (view colorGreen) (view colorYellow)
inactiveLockedLO1Context = LO1DrawContext (view colorWhite) (view colorRed) (view colorGreen) (view colorYellow)

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

confirmCommand input command alternative = do
    maybeUpdateStr <- input
    case maybeUpdateStr of
        Just "yes" -> command
        _ -> alternative

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

instance (Selector IO (ScopedSelector scopeType) row, Eq row, StringSettable scopeType) =>
        App (Projection (ScopedSelector scopeType) row) where
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

instance (Selector IO ios1 row1, Selector IO (ScopedSelector scopeType) row2, Scopeable row1 scopeType, Eq row1, Eq row2) =>
            App (ProjectionPair ios1 row1 (ScopedSelector scopeType) row2) where
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
