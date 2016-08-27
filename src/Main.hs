{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import UI.NCurses
import Database.PostgreSQL.Simple
import Projection
import Accounts
import AccountSelector
import TransactionSelector
import Data.List.Zipper
import Control.Lens
import Control.Monad.IO.Class (liftIO)
import Control.Exception
import Data.ByteString.Char8 (unpack)


data AppState a row = AppState {
    _state_ilo1 :: ILO1 a row,
    _state_ilo1_a :: a,
    _state_w :: Window,
    _state_colors :: ColorID,
    _state_status :: String,
    _stateStatusWindow :: Window,
    _stateInputWindow :: Window
    }

makeLenses ''AppState

main :: IO ()
main = do
    conn <- connectPostgreSQL "dbname='accounts' user='matthew' password='matthew'"
    let accountsProj = Projection (LO1 (fromList []) (fromList account_alenses)) account_selector conn
    let transactionProj = Projection (LO1 (fromList []) (fromList transactionAlenses)) transactionSelector conn
    -- proj <- (projection_accounts_i1 ^. i1_select) proj
    runCurses $ do
        setEcho False
        setCursorMode CursorInvisible
        cid <- newColorID ColorRed ColorDefault 2
        max_y <- fmap (fromInteger . fst) screenSize
        max_x <- fmap (fromInteger . snd) screenSize
        w <- newWindow (max_y - 4) (max_x - 2) 1 1
        inputWindow <- newWindow 1 (max_x - 1) (max_y - 3) 1
        statusWindow <- newWindow 1 (max_x - 1) (max_y - 2) 1
        mainLoop (AppState projectionILO1 transactionProj w cid "Welcome" statusWindow inputWindow )

-- algebraic data type for current view (this or that or...)
mainLoop :: AppState a row -> Curses ()
mainLoop state = do
    updateWindow (state ^. stateStatusWindow) $ do
        clear
        max_x <- fmap (fromInteger . snd) windowSize
        drawStringPos (clipString (max_x-1) (state ^. state_status)) 0 0
    updateWindow (state ^. stateInputWindow) $ do
        clear
    drawLO1 (state ^. state_w) ((state ^. state_ilo1 ^. ilo1_lo1f) (state ^. state_ilo1_a)) (state ^. state_colors)
    render
    ev <- getEvent (state ^. state_w) Nothing
    case ev of
        Just (EventCharacter 'q') -> return ()
        Just (EventCharacter 'k') -> mainLoop (state & state_ilo1_a %~ (state ^. state_ilo1 ^. ilo1_i1 ^. i1_up))
        Just (EventCharacter 'j') -> mainLoop (state & state_ilo1_a %~ (state ^. state_ilo1 ^. ilo1_i1 ^. i1_down))
        Just (EventCharacter 'h') -> mainLoop (state & state_ilo1_a %~ (state ^. state_ilo1 ^. ilo1_i1 ^. i1_left))
        Just (EventCharacter 'l') -> mainLoop (state & state_ilo1_a %~ (state ^. state_ilo1 ^. ilo1_i1 ^. i1_right))
        Just (EventCharacter 's') -> do
            new_state <- liftIO (catch (do
                    new_ilo1_a <- (state ^. state_ilo1 ^. ilo1_i1 ^. i1_select) (state ^. state_ilo1_a)
                    return (state & state_ilo1_a .~ new_ilo1_a))
                (\e -> do
                    let err = show (e :: SqlError)
                    return (state & state_status .~ (unpack $ sqlErrorMsg e))))
            mainLoop new_state
        Just (EventCharacter 'i') -> do
            new_ilo1_a <- liftIO $ (state ^. state_ilo1 ^. ilo1_i1 ^. i1_insert) (state ^. state_ilo1_a)
            mainLoop (state & state_ilo1_a .~ new_ilo1_a)
        Just (EventCharacter 'u') -> do
            max_y <- updateWindow (state ^. state_w) $ do
                max_y <- fmap (fromInteger . fst) windowSize
                return max_y
            update_str <- getString (state ^. stateInputWindow) "?" 0 0
            new_state <- liftIO (catch (do
                    new_ilo1_a <- (state ^. state_ilo1 ^. ilo1_i1 ^. i1_update) (state ^. state_ilo1_a) update_str
                    return (state & state_ilo1_a .~ new_ilo1_a))
                (\e -> do
                    let err = show (e :: SqlError)
                    return (state & state_status .~ (unpack $ sqlErrorMsg e))))
            mainLoop new_state
        Just (EventCharacter 'd') -> do
            new_ilo1_a <- liftIO $ (state ^. state_ilo1 ^. ilo1_i1 ^. i1_delete) (state ^. state_ilo1_a)
            mainLoop (state & state_ilo1_a .~ new_ilo1_a)
        Just (EventCharacter 'c') -> mainLoop (state & state_status .~ "")
        _ -> mainLoop state

waitFor :: Window -> (Event -> Bool) -> Curses ()
waitFor w p = loop where
    loop = do
        ev <- getEvent w Nothing
        case ev of
            Nothing -> loop
            Just ev' -> if p ev' then return () else loop

fmaparg a m = fmap (\f -> f a) m

getString w s y x = do
    updateWindow w $ do
        moveCursor y x
        drawString s
    getStringLoop w [] y (x + (toInteger (length s)) + 1)

getStringLoop w s y x = do
    render
    ev <- getEvent w Nothing
    case ev of
        Just (EventSpecialKey KeyBackspace) -> case s of
            [] -> getStringLoop w s y x
            s -> do
                updateWindow w $ do
                    moveCursor y (x-1)
                    drawString " "
                    moveCursor y (x-1)
                getStringLoop w (tail s) y (x-1)
        Just (EventCharacter '\n') -> return (reverse s)
        Just (EventCharacter c) -> do
            updateWindow w $ do
                moveCursor y x
                drawString [c]
            getStringLoop w (c:s) y (x+1)
        _ -> getStringLoop w s y x

-- LO1RowToStrings :: (LO1 row) -> row -> [String]
lO1RowToStrings lo1 r = fmap (\lens -> (lens ^. alens_get) r) (toList $ lo1 ^. lo1_zip_lens)

--LO1ToStrings :: LO1 row -> Int -> Int -> ([String] [[String]] [[String]])
--LO1ToStrings lo1 n_above n_below = 

drawError :: Update ()
drawError = do
    moveCursor 1 1
    drawString "!"

inmap fns args = [f a | (f, a) <- zip fns args]

maxLength = 25

clipString :: Int -> String -> String
clipString size str = if (length str) > size then ((take (size - 3) str) ++ "...") else str

drawGlyphPos glyph (y, x) = do
    moveCursor y x
    drawGlyph glyph

drawStringPos s y x = do
    moveCursor y x
    drawString s

--drawString [] (y, x) = return ()
--drawString (glyph:rest) (y, x) = do
--    drawGlyphPos glyph (y, x)
--    drawString rest (y, x+1)

drawRow j = do
    max_x <- fmap (fromInteger . snd) windowSize
    let drawings = fmap (drawGlyphPos glyphLineH) (zip (repeat j) [0..(max_x-1)])
    foldl1 (>>) drawings

drawColumn i = do
    max_y <- fmap (fromInteger . fst) windowSize
    let drawings = fmap (drawGlyphPos glyphLineV) (zip [0..(max_y-2)] (repeat i))
    foldl1 (>>) drawings

drawCross coords = do
    max_y <- fmap (fromInteger . fst) windowSize
    max_x <- fmap (fromInteger . snd) windowSize
    if coords == (0, 0) then drawGlyphPos glyphCornerUL coords
    else if coords == (0, max_x - 1) then drawGlyphPos glyphCornerUR coords
    else if coords == (max_y - 2, 0) then drawGlyphPos glyphCornerLL coords
    else if coords == (max_y - 2, max_x - 1) then drawGlyphPos glyphCornerLR coords
    else if fst coords == 0 then drawGlyphPos glyphTeeT coords
    else if fst coords == max_y - 2 then drawGlyphPos glyphTeeB coords
    else if snd coords == 0 then drawGlyphPos glyphTeeL coords
    else if snd coords == max_x - 1 then drawGlyphPos glyphTeeR coords
    else drawGlyphPos glyphPlus coords

drawGrid rows cols = do
    let row_drawings = fmap drawRow rows
    let col_drawings = fmap drawColumn cols
    let cross_drawings = fmap drawCross [(i,j) | i <- rows, j <- cols]
    foldl1 (>>) (row_drawings ++ col_drawings ++ cross_drawings)

--drawStrings strs starts row = do

applyZipperY (Zip ls []) cursor_start max_y = Zip [fmaparg a r | (r, a) <- (zip ls (reverse [1..(cursor_start-3)]))] []
applyZipperY (Zip ls (curs:rs)) cursor_start max_y = Zip [fmaparg a r | (r, a) <- (zip ls (reverse [1..(cursor_start-3)]))] ((fmaparg cursor_start curs):[fmaparg a r | (r, a) <- (zip rs [(cursor_start+2)..(max_y-3)])])

applyZipperX z xs = fmap (\r -> [f x | (f, x) <- (zip r xs)]) z

-- drawLO1 :: Show row => Window -> (LO1 row) -> Curses ()
drawLO1 w lo1 cid = updateWindow w $ do
    clear
    max_y <- fmap fst windowSize
    max_x <- fmap snd windowSize
    if max_y < 8
        then drawError
        else do
            let n_above = quot (max_y - 6) 2
            let n_below = (quot (max_y - 5) 2) - 1 -- not using last row right now
            let lens_strings = fmap (view alens_name) (toList $ lo1 ^. lo1_zip_lens)
            let lens_lengths = fmap length lens_strings
            let row_strings = fmap (lO1RowToStrings lo1) (lo1 ^.lo1_zip_row)
            let row_strings_list = toList row_strings
            let row_lengths = fmap (fmap (max . length)) row_strings_list
            let maxs = foldr inmap lens_lengths row_lengths
            let lengths = fmap (min maxLength) maxs
            let row_strings_clipped = fmap (inmap (fmap clipString lengths)) row_strings
            let gridRows = [0, n_above + 1, n_above + 4, max_y - 2]
            let gridColsWidths = fmap (+ 3) lengths
            let gridColsInts = fmap (\b -> (foldl (+) 0 (take b gridColsWidths))) [0..(length  lengths)]
            let gridCols = (fmap toInteger gridColsInts) ++ [max_x - 1]
            let strStarts = [x+2 | x <- gridCols]
            let w2 = fmap (fmap drawStringPos) row_strings_clipped
            let w3 = applyZipperY w2 (quot max_y 2) max_y
            let w4 = applyZipperX w3 strStarts
            drawGrid gridRows gridCols
            let q1 = fmap drawStringPos lens_strings
            let q2 = fmaparg ((quot max_y 2) - 1) q1
            let q3 = [f a | (f, a) <- (zip q2 strStarts)] 
            foldl (>>) (return ()) q3
            setColor cid
            let qq0 = [(lens, x) | (lens, x) <- zip (toList $ lo1 ^. lo1_zip_lens) strStarts]
            let qq1 = filter (\(lens, x) -> lens == (cursor (lo1 ^. lo1_zip_lens))) qq0
            let qq2 = fmap (\(lens, x) -> drawStringPos (lens ^. alens_name) ((quot max_y 2) - 1) x) qq1
            foldl (>>) (return ()) qq2
            setColor defaultColorID
            foldl (>>) (return ()) (toList (fmap (foldl (>>) (return ())) w4))
