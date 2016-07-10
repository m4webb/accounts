module Projection where

import Accounts
import AccountsLenses
import Database.PostgreSQL.Simple

data Projection row = Projection (LO1 row) (IOSelector row) Connection

-- implement I1

projection_i1_select :: Projection -> IO Projection
projection_i1_select Projection (LO1 zl_row zl_lens) selector conn = do
    rows <- ios_select selector conn
    return Projection (LO1 (ZipList (head rows) [] (tail rows)) zl_lens) selector conn

projection_i1_insert :: Projection -> IO Projection
projection_i1_insert Projection (LO1 zl_row zl_lens) selector conn = do
    new_row <- ios_insert selector conn
    return Projection (LO1 (zl_append new_row zl_row) zl_lens) selector conn

projection_i1_update :: Projection -> String -> IO Projection
projection_i1_update (Projection (LO1 zl_row zl_lens) selector conn) value = do
    case lens_set_ of
        Nothing -> Projection (LO1 zl_row zl_lens) selector conn
        Just setter -> do
            updated_row = setter row value conn
            return Projection (LO1 (ZipList updated_row b_row a_row) zl_lens) selector conn
    where ZipList row b_rows a_rows = zl_row
          ZipList lens _ _ = zl_lens
          lens_set_ = lens_set zl_lens

projection_i1_delete :: Projection -> IO Projection
projection_i1_delete Projection (LO1 (ZipList curr_row b_rows next_row:a_rows) zl_lens) selector conn = do
    ios_delete curr_row
    return Projection (LO1 (ZipList next_row b_rows a_rows) zl_lens) selector conn
projection_i1_delete Projection (LO1 (ZipList curr_row prev_row:b_rows []) zl_lens selector conn = do
    ios_delete curr_Row
    return Projection (LO1 (ZipList prev_row b_rows []) zl_lens) selector conn
projection_i1_delete proj = IO proj

