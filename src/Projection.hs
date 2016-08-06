{-# LANGUAGE TemplateHaskell #-}

module Projection where

import Accounts
import AccountsSelector
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

-- implement I1

projection_accounts_i1 = I1
    projection_accounts_i1_select
    projection_accounts_i1_insert
    projection_accounts_i1_update
    projection_accounts_i1_delete
    projection_accounts_i1_up
    projection_accounts_i1_down
    projection_accounts_i1_left
    projection_accounts_i1_right

projection_accounts_i1_select self = do
    rows <- (self ^. proj_ios) ^. ios_select $ (self ^. proj_conn)
    return $ self & proj_zip_row .~ (fromList rows)

projection_accounts_i1_insert self = do
    new_row <- (self ^. proj_ios) ^. ios_insert $ (self ^. proj_conn) 
    return $ self & proj_zip_row %~ end . insert new_row

projection_accounts_i1_update self value = do
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
                            ((self ^. proj_ios) ^. ios_update) (self ^. proj_conn) new_row
                            return $ self & proj_zip_row %~ replace new_row

projection_accounts_i1_delete self = do
    let m_curr_row = safeCursor (self ^. (proj_lo1 . lo1_zip_row))
    case m_curr_row of
        Nothing -> return self
        Just curr_row -> do
            ((self ^. proj_ios) ^. ios_delete) (self ^. proj_conn) curr_row
            return $ self & proj_zip_row %~ pop

projection_accounts_i1_up self = self & proj_zip_row %~ left

projection_accounts_i1_down self = self & proj_zip_row %~ right

projection_accounts_i1_left self = self & proj_zip_lens %~ left

projection_accounts_i1_right self = self & proj_zip_lens %~ right 
