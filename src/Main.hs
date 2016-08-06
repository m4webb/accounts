{-# LANGUAGE OverloadedStrings #-}

module Main where

import UI.NCurses
import Database.PostgreSQL.Simple
import Projection
import Accounts
import AccountsSelector
import Data.List.Zipper
import Control.Lens

main :: IO ()
main = do
    conn <- connectPostgreSQL "dbname='accounts' user='matthew' password='matthew'"
    let proj = Projection (LO1 (fromList []) (fromList account_alenses)) account_selector conn
    proj <- (projection_accounts_i1 ^. i1_select) proj
    runCurses $ do
        setEcho False
        w <- defaultWindow
        --updateWindow w $ do
        --    moveCursor 1 10
        --    drawString "Hello world!"
        --    moveCursor 3 10
        --    drawString "(press q to quit)"
        --    moveCursor 0 0
        drawLO1 w (proj ^. proj_lo1)
        render
        waitFor w (\ev -> ev == EventCharacter 'q' || ev == EventCharacter 'Q')

waitFor :: Window -> (Event -> Bool) -> Curses ()
waitFor w p = loop where
    loop = do
        ev <- getEvent w Nothing
        case ev of
            Nothing -> loop
            Just ev' -> if p ev' then return () else loop

-- LO1RowToStrings :: (LO1 row) -> row -> [String]
lO1RowToStrings lo1 r = fmap (\lens -> (lens ^. alens_get) r) (toList $ lo1 ^. lo1_zip_lens)

--LO1ToStrings :: LO1 row -> Int -> Int -> ([String] [[String]] [[String]])
--LO1ToStrings lo1 n_above n_below = 

drawError :: Update ()
drawError = do
    moveCursor 1 1
    drawString "!"

after (Zip b a) = a

-- drawLO1 :: Show row => Window -> (LO1 row) -> Curses ()
drawLO1 w lo1 = updateWindow w $ do
    max_y <- fmap fst windowSize
    if max_y < 8
        then drawError
        else do
            let n_above = quot (max_y - 6) 2
            let n_below = quot (max_y - 5) 2
            let lens_lens = after (start (fmap (length . view alens_name) (lo1 ^. lo1_zip_lens)))
            let zip_strings = fmap (lO1RowToStrings lo1) (lo1 ^.lo1_zip_row)
            let zip_maxs = fmap (fmap (max . length)) zip_strings
            let g x y = [f a | (f, a) <- zip x y]
            let maxs = foldr g lens_lens (after (start zip_maxs))
            moveCursor 1 1
            drawString (show zip_strings)
            moveCursor 4 1
            drawString (show lens_lens)
            moveCursor 7 1
            drawString (show maxs)
