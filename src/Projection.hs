{-# LANGUAGE TemplateHaskell #-}

module Projection where

import Accounts
import AccountsLenses
import Database.PostgreSQL.Simple
import Control.Lens

infixl 1 $$ -- object method
($$) :: a -> (a -> b) -> b
x $$ f = f x

data Projection row = Projection {
    _proj_lo1 :: LO1 row,
    _proj_ios :: IOSelector row,
    _proj_conn :: Connection
    }
makeLenses ''Projection

-- implement I1

instance I1 (Projection row) where

    i1_select self = do
        -- python: rows = self.ios.select(self.conn)
        rows <- (self ^. proj_ios) $$ ios_select $ (self ^. proj_conn)
        return $ (proj_lo1 . lo1_zl_row) .~ ZipList (head rows) [] (tail rows) $ self

--  i1_insert proj = do
--      new_row <- ios_insert (proj_get_ios proj) (proj_get_conn conn)
--      let zl_row = lo1_get_zl_row . proj_get_zl proj -- here; lenses
--      return proj_set_lo1 . lo1_set_zl_row zl_append new_row zl_row

--  i1_update proj value = do
--      let lset = proj_get_lo1 . lo1_get_zl_lens . zl_get_item . lens_get_set 
--      case lens_set_ of
--          Nothing -> Projection (LO1 zl_row zl_lens) selector conn
--          Just setter -> do
--              updated_row = setter row value conn
--              return Projection (LO1 (ZipList updated_row b_row a_row) zl_lens) selector conn
--      where ZipList row b_rows a_rows = zl_row
--            ZipList lens _ _ = zl_lens
--            lens_set_ = lens_set zl_lens

--  i1_delete Projection (LO1 (ZipList curr_row b_rows next_row:a_rows) zl_lens) selector conn = do
--      ios_delete curr_row
--      return Projection (LO1 (ZipList next_row b_rows a_rows) zl_lens) selector conn
--  i1_delete Projection (LO1 (ZipList curr_row prev_row:b_rows []) zl_lens selector conn = do
--      ios_delete curr_Row
--      return Projection (LO1 (ZipList prev_row b_rows []) zl_lens) selector conn
--  i1_delete proj = IO proj

