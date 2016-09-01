{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import UI.NCurses
import UI.NCurses.Types
import Database.PostgreSQL.Simple
import Projection
import Accounts
import AccountSelector
import TransactionSelector
import SplitSelector
import Data.List.Zipper
import Control.Lens
import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Char8 (unpack)
import Control.Monad.Catch
import Control.Exception (throwIO)
import Filter

-- TODO: State monad

--data TransactionState = TransactionBegin | TransactionError

--instance Show TransactionState where
--    show TransactionBegin = "TransactionBegin"
--    show TransactionError = "TransactionError"

-- allow exceptions to be caught in Curses monad

instance MonadThrow Curses where
    throwM e = Curses (throwIO e)

instance MonadCatch Curses where
    catch (Curses a) handler = Curses $ catch a (\e -> unCurses (handler e))

data CoreColors = CoreColors {
    _coreColorRed :: ColorID
    }

makeLenses ''CoreColors

data BasicState = 
        BasicStateSelect |
        BasicStateCommit |
        BasicStateRollback |
        BasicStateNormal |
        BasicStateError |
        BasicStateQuit
        deriving (Show)


data CoreState = CoreState {
    _coreMainWindow :: Window,
    _coreStatusWindow :: Window,
    _coreInputWindow :: Window,
    _coreConnection :: Connection,
    _coreColors :: CoreColors,
    _coreStatus :: String,
    --_coreTransactionState :: TransactionState,
    _coreBasicState :: BasicState
    }

makeLenses ''CoreState

data SimpleProjectionApp a row = SimpleProjectionApp {
    _spaILO1 :: ILO1 a row,
    _spaA :: a
    }

makeLenses ''SimpleProjectionApp

data CoreControl = CoreControl {
    _coreDraw :: CoreState -> Curses (),
    _coreHandleEvent :: Event -> CoreState -> Maybe (Curses CoreState)
    }

makeLenses ''CoreControl

data AppControl a = AppControl {
    _appDraw :: Window -> CoreColors -> a -> Curses (),
    _appHandleEvent :: Event -> (Curses String) -> a -> Curses a,
    _appSelect :: a -> IO a
    }

makeLenses ''AppControl

data BigState a = BigState {
    _coreControl :: CoreControl,
    _coreState :: CoreState,
    _appControl :: AppControl a,
    _appState :: a
    }

makeLenses ''BigState

accountProj conn = Projection (LO1 (fromList []) (fromList account_alenses) (fromList [])) account_selector conn
transactionProj conn = Projection (LO1 (fromList []) (fromList transactionAlenses) (fromList [])) transactionSelector conn
splitProj conn = Projection (LO1 (fromList []) (fromList splitAlenses) (fromList [])) splitSelector conn

basicCoreDraw core = do
    updateWindow (core ^. coreStatusWindow) $ do
        clear
        max_x <- fmap (fromInteger . snd) windowSize
        let msg = (show (core ^. coreBasicState)) ++ " " ++ (core ^. coreStatus)
        drawStringPos (clipString (max_x-1) msg) 0 2
    updateWindow (core ^. coreInputWindow) $ do
        clear

basicCoreHandleEvent :: Event -> CoreState -> Maybe (Curses CoreState)
basicCoreHandleEvent event core = do
    case event of
        --EventCharacter '1' -> Just (liftIO $ changeSpa core accountProj)
        --EventCharacter '2' -> Just (liftIO $ changeSpa core transactionProj)
        --EventCharacter '3' -> Just (liftIO $ changeSpa core splitProj)
        EventCharacter 'r' -> Just (return (core & coreBasicState .~ BasicStateRollback))
        EventCharacter 'c' -> Just (return (core & coreBasicState .~ BasicStateCommit))
        EventCharacter 'q' -> Just (return (core & coreBasicState .~ BasicStateQuit))
        EventCharacter 'b' -> Just (return (core & coreStatus .~ ""))
        _ -> Nothing

spaProcessEvent :: Event -> Curses String -> SimpleProjectionApp a row -> Curses (SimpleProjectionApp a row)
spaProcessEvent event str spa = do
    case event of
        EventCharacter 'k' -> return $ spa & spaA %~ (spa ^. spaILO1 ^. ilo1_i1 ^. i1_up)
        EventCharacter 'j' -> return $ spa & spaA %~ (spa ^. spaILO1 ^. ilo1_i1 ^. i1_down)
        EventCharacter 'h' -> return $ spa & spaA %~ (spa ^. spaILO1 ^. ilo1_i1 ^. i1_left)
        EventCharacter 'l' -> return $ spa & spaA %~ (spa ^. spaILO1 ^. ilo1_i1 ^. i1_right)
        EventCharacter 'w' -> return $ spa & spaA %~ (spa ^. spaILO1 ^. ilo1_i1 ^. i1_switch)
        EventCharacter 's' -> liftIO $ spa & spaA %%~ (spa ^. spaILO1 ^. ilo1_i1 ^. i1_select)
        EventCharacter 'i' -> liftIO $ spa & spaA %%~ (spa ^. spaILO1 ^. ilo1_i1 ^. i1_insert)
        EventCharacter 'u' -> do
            updateStr <- str
            liftIO $ spa & spaA %%~ ((spa ^. spaILO1 ^. ilo1_i1 ^. i1_update) updateStr)
        EventCharacter 'd' -> liftIO $ spa & spaA %%~ (spa ^. spaILO1 ^. ilo1_i1 ^. i1_delete)
        EventCharacter 'f' -> do
            filterStr <- str
            return $ spa & spaA %~ ((spa ^. spaILO1 ^. ilo1_i1 ^. i1_set_filters) [Filter filterStr])
        EventCharacter 'F' -> return $ spa & spaA %~ (spa ^. spaILO1 ^. ilo1_i1 ^. i1_reset_filters)
        _ -> return spa

spaSelect spa = spa & spaA %%~ (spa ^. spaILO1 ^. ilo1_i1 ^. i1_select)

spaDraw window colors spa = drawLO1 window colors ((spa ^. spaILO1 ^. ilo1_lo1f) (spa ^. spaA))

initBigState state = do
    liftIO $ begin (state ^. coreState ^. coreConnection)
    let spa = state ^. appState
    spaSelected <- liftIO $ spa & spaA %%~ (spa ^. spaILO1 ^. ilo1_i1 ^. i1_select)
    let state1 = state & appState .~ spaSelected
    return state1

--changeSpa state projFactory = do
--    let newProj = SimpleProjectionApp projectionILO1 (projFactory (state ^. coreState ^. coreConnection))
--    let newState = state & appState .~ newProj
--    let newState1 = newState & coreState . coreTransactionState .~ TransactionBegin
--    let spa = newState1 ^. appState
--    rollback (newState1 ^. coreState ^. coreConnection)
--    begin (newState1 ^. coreState ^. coreConnection)
--    newSpa <- spa & spaA %%~ (spa ^. spaILO1 ^. ilo1_i1 ^. i1_select)
--    let newState2 = newState1 & appState .~ newSpa
--    return newState2

handleSqlError core error = do
    case (sqlExecStatus error) of
        NonfatalError -> do
            let newCore = core & coreStatus .~ (unpack (sqlErrorMsg error))
            return newCore
        _ -> do
            let newCore = core & coreStatus .~ (unpack (sqlErrorMsg error))
            let newCore1 = newCore & coreBasicState .~ BasicStateError
            return newCore1

-- can I assume that select doesn't fail?

--rollbackState core = do
--    rollback (core ^. coreConnection)
--    return (core & coreBasicState .~ BasicStateSelect)

--commitState state = do
    -- need to catch here? what happens when commit fails?
--    commit (state ^. coreState ^. coreConnection)
--    begin (state ^. coreState ^. coreConnection)
--    newState <- state & appState . spaA %%~ (state ^. appState ^. spaILO1 ^. ilo1_i1 ^. i1_select)
--    let newState1 = newState & coreState . coreTransactionState .~ TransactionBegin
--    let newState2 = newState1 & coreState . coreStatus .~ ""
--    return newState2

--commitStateSafe state = catch (commitState state) (handleSqlError state)

main :: IO ()
main = do
    conn <- connectPostgreSQL "dbname='accounts' user='matthew' password='matthew'"
    runCurses $ do
        setEcho False
        setCursorMode CursorInvisible
        colorRedID <- newColorID ColorRed ColorDefault 2
        let colors = CoreColors colorRedID
        max_y <- fmap (fromInteger . fst) screenSize
        max_x <- fmap (fromInteger . snd) screenSize
        mainWindow <- newWindow (max_y - 4) (max_x - 2) 1 1
        inputWindow <- newWindow 1 (max_x - 1) (max_y - 3) 1
        statusWindow <- newWindow 1 (max_x - 1) (max_y - 2) 1
        let coreControl_ = CoreControl basicCoreDraw basicCoreHandleEvent
        let coreState_ = CoreState mainWindow statusWindow inputWindow conn colors "Welcome" BasicStateSelect
        let appControl_ = AppControl spaDraw spaProcessEvent spaSelect
        let appState_ = SimpleProjectionApp projectionILO1 (splitProj conn)
        bigState <- initBigState (BigState coreControl_ coreState_ appControl_ appState_)
        mainLoop bigState

commitUnsafe core = do
    liftIO $ commit (core ^. coreConnection)
    let newCore1 = core & coreBasicState .~ BasicStateSelect
    let newCore2 = newCore1 & coreStatus .~ ""
    return newCore2

mainLoop :: BigState a -> Curses ()
mainLoop state = do
    let core = state ^. coreState
    case (core ^. coreBasicState) of
        BasicStateSelect -> do
            liftIO $ begin (core ^. coreConnection)
            newAppState <- liftIO $ (state ^. appControl ^. appSelect) (state ^. appState)
            let newState1 = state & coreState . coreStatus .~ ""
            let newState2 = newState1 & coreState . coreBasicState .~ BasicStateNormal
            let newState3 = newState2 & appState .~ newAppState
            mainLoop newState3
        BasicStateCommit -> do
            newCoreState <- catch (commitUnsafe core) (handleSqlError core)
            mainLoop (state & coreState .~ newCoreState)
        BasicStateRollback -> do
            liftIO $ rollback (core ^. coreConnection)
            let newState = state & coreState . coreBasicState .~ BasicStateSelect
            mainLoop newState
        BasicStateNormal -> do
            (state ^. coreControl ^. coreDraw) core
            (state ^. appControl ^. appDraw) (core ^. coreMainWindow) (core ^. coreColors) (state ^. appState)
            render
            event <- getEvent (core ^. coreMainWindow) Nothing
            newState <- case event of
                Nothing -> return state
                Just event -> do
                    let maybeIONewCoreState = (state ^. coreControl ^. coreHandleEvent) event core
                    case maybeIONewCoreState of
                        Just iONewCoreState -> do
                            newCoreState <- iONewCoreState
                            return (state & coreState .~ newCoreState) 
                        Nothing -> do
                            let cursesStr = getString (core ^. coreInputWindow) "?" 0 2
                            let iONewAppState = (state ^. appControl ^. appHandleEvent) event cursesStr (state ^. appState)
                            let iONewState = iONewAppState >>= (\newAppState -> return (state & appState .~ newAppState))
                            catch iONewState (\e -> (state & coreState %%~ (\core -> handleSqlError core e)))
            mainLoop newState
        BasicStateError -> do
            (state ^. coreControl ^. coreDraw) core
            (state ^. appControl ^. appDraw) (core ^. coreMainWindow) (core ^. coreColors) (state ^. appState)
            render
            event <- getEvent (core ^. coreMainWindow) Nothing
            case event of
                Just (EventCharacter 'r') -> mainLoop (state & coreState . coreBasicState .~ BasicStateRollback)
                _ -> mainLoop state
        BasicStateQuit -> return ()

--

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
    max_x <- updateWindow w (fmap (fromInteger . snd) windowSize)
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
            if (x < (max_x -2)) -- don't draw off window, caps max size of return string; why not scroll string?
                then do
                    updateWindow w $ do
                        moveCursor y x
                        drawString [c]
                    getStringLoop w (c:s) y (x+1)
                else
                    getStringLoop w s y x
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

drawLO1 w colors lo1 = updateWindow w $ do
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
            setColor (colors ^. coreColorRed)
            let qq0 = [(lens, x) | (lens, x) <- zip (toList $ lo1 ^. lo1_zip_lens) strStarts]
            let qq1 = filter (\(lens, x) -> lens == (cursor (lo1 ^. lo1_zip_lens))) qq0
            let qq2 = fmap (\(lens, x) -> drawStringPos (lens ^. alens_name) ((quot max_y 2) - 1) x) qq1
            foldl (>>) (return ()) qq2
            setColor defaultColorID
            foldl (>>) (return ()) (toList (fmap (foldl (>>) (return ())) w4))
            drawStringPos (clipString (fromInteger (max_x-3))
                ("Filter " ++ filtersToSql (toList (lo1 ^. lo1_zip_filters)))) (max_y-1) 2

