{-# LANGUAGE TemplateHaskell #-}

module Projection where

import Accounts
import Database.PostgreSQL.Simple
import Data.List.Zipper
import Control.Lens

data Projection row = Projection {
    _proj_lo1 :: LO1 row,
    _proj_ios :: IOSelector row,
    _proj_conn :: Connection
    }

makeLenses ''Projection

proj_zip_row = proj_lo1 . lo1_zip_row
proj_zip_lens = proj_lo1 . lo1_zip_lens

rightCheck (Zip [] []) = Zip [] []
rightCheck (Zip (x:ls) []) = Zip ls [x]
rightCheck z = z

-- implement I1

--projection_i1 = I1
--    projection_i1_select
--    projection_i1_insert
--    projection_i1_update
--    projection_i1_delete
--    projection_i1_set_filters
--    projection_i1_reset_filters
--    projection_i1_up
--    projection_i1_down
--    projection_i1_left
--    projection_i1_right
--    projection_i1_switch

instance HasLO1 Projection where
    getLO1 proj = proj ^. proj_lo1

instance I1 (Projection row) where
    i1_select self = do
        let filters = toList (self ^. proj_lo1 ^. lo1_zip_filters)
        rows <- ((self ^. proj_ios) ^. ios_select) (self ^. proj_conn) filters
        return $ self & proj_zip_row .~ (fromList rows)

    i1_insert self = do
        new_row <- (self ^. proj_ios) ^. ios_insert $ (self ^. proj_conn) 
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
                                new_new_row <- ((self ^. proj_ios) ^. ios_update) (self ^. proj_conn) new_row
                                return $ self & proj_zip_row %~ replace new_new_row

    i1_delete self = do
        let m_curr_row = safeCursor (self ^. (proj_lo1 . lo1_zip_row))
        case m_curr_row of
            Nothing -> return self
            Just curr_row -> do
                ((self ^. proj_ios) ^. ios_delete) (self ^. proj_conn) curr_row
                return $ self & proj_zip_row %~ pop . right

    i1_set_filters self filters = self & proj_lo1 . lo1_zip_filters .~ fromList filters

    i1_reset_filters self = self & proj_lo1 . lo1_zip_filters .~ fromList [] 

    i1_up self = self & proj_zip_row %~ left

    i1_down self = self & proj_zip_row %~ rightCheck . right

    i1_left self = self & proj_zip_lens %~ left

    i1_right self = self & proj_zip_lens %~ rightCheck . right

    i1_switch self = self

-- projectionILO1 = ILO1 projection_i1 (\p -> (p ^. proj_lo1))
