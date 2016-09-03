{-# LANGUAGE TemplateHaskell #-}

module Projection where

import Accounts
import Database.PostgreSQL.Simple
import Data.List.Zipper
import Control.Lens

data Projection ios row = Projection {
    _proj_ios :: ios,
    _proj_lo1 :: LO1 row
    }

makeLenses ''Projection

proj_zip_row = proj_lo1 . lo1_zip_row
proj_zip_lens = proj_lo1 . lo1_zip_lens

rightCheck (Zip [] []) = Zip [] []
rightCheck (Zip (x:ls) []) = Zip ls [x]
rightCheck z = z

instance HasLO1 (Projection ios) where
    getLO1 proj = proj ^. proj_lo1

instance (IOSelector ios row) => I1 (Projection ios row) where
    i1_select self = do
        rows <- iosSelect (self ^. proj_ios)
        return $ self & proj_zip_row .~ (fromList rows)

    i1_insert self = do
        new_row <- iosInsert (self ^. proj_ios)
        return $ self & proj_zip_row %~ (insert new_row . end)

    i1_update self value = do
        let m_curr_row = safeCursor (self ^. (proj_lo1 . lo1_zip_row))
        case m_curr_row of
            Nothing -> return self
            Just curr_row -> do
                let m_curr_lens = safeCursor (self ^. (proj_lo1 . lo1_zip_lens))
                case m_curr_lens of
                    Nothing -> return self
                    Just curr_lens -> do
                        let m_setter = curr_lens ^. alens_set
                        case m_setter of
                            Nothing -> return self
                            Just setter -> do
                                let new_row = setter curr_row value
                                new_new_row <- iosUpdate (self ^. proj_ios) new_row
                                return $ self & proj_zip_row %~ replace new_new_row

    i1_delete self = do
        let m_curr_row = safeCursor (self ^. (proj_lo1 . lo1_zip_row))
        case m_curr_row of
            Nothing -> return self
            Just curr_row -> do
                iosDelete (self ^. proj_ios) curr_row
                return $ self & proj_zip_row %~ pop . right

    i1_set_filters self filters = return $ self & proj_lo1 . lo1_zip_filters .~ fromList filters

    i1_reset_filters self = return $ self & proj_lo1 . lo1_zip_filters .~ fromList [] 

    i1_up self = return $ self & proj_zip_row %~ left

    i1_down self = return $ self & proj_zip_row %~ rightCheck . right

    i1_left self = return $ self & proj_zip_lens %~ left

    i1_right self = return $ self & proj_zip_lens %~ rightCheck . right
