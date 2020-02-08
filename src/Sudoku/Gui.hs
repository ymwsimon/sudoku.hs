{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

module Sudoku.Gui where
import Control.Concurrent (forkIO, threadDelay)
import Control.Exception (catch)
import Control.Monad (forM_, forM, when, unless)
import Data.Array.Unboxed
import Data.GI.Base
import Data.GI.Base.GError (gerrorMessage, GError(..))
import Data.Int
import Data.IORef
import Data.Maybe (fromJust, fromMaybe)
import Data.GI.Base.BasicTypes
import GI.Gtk hiding (main, init)
import Data.GI.Base.GValue (toGValue, fromGValue)
import GI.Gtk.Objects.Widget
import GI.Pango.Structs.FontDescription
import GI.Pango.Objects.FontMap
import GI.Gtk.Objects.Container
import Paths_sudoku
import Sudoku.Sudoku
import System.Random
import qualified Data.Text as T (unpack, Text, pack, empty)
import qualified GI.Gtk as Gtk (main, init)
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr
import Foreign.ForeignPtr
import GI.Pango.Objects.FontMap
import Unsafe.Coerce
import Text.Read

data Env = Env { difficulty :: Int
               , winWidth   :: Int
               , winHeight  :: Int
               , pulseDuration :: Int }

cssPath :: T.Text
cssPath = "ui/gui.css"

uiPath :: T.Text
uiPath = "ui/ui.glade"

dfltEnv :: Env
dfltEnv = Env 50 635 540 350000

-- | view coordinate space as list
cSpace :: [(Int, Int)]
cSpace = flip (,) <$> [1..sudokuSz] <*> [1..sudokuSz]

buildWindow :: Int -> Int -> IO Window
buildWindow (fromIntegral -> w) (fromIntegral -> h) = do
    win <- new Window [#title := "sudokuhs", #resizable := True]
    on win #destroy mainQuit
    #resize win w h
    return win

applyCss :: Window -> IO ()
applyCss win = do
    screen <- windowGetScreen win
    css <- cssProviderNew
    cssProviderLoadFromPath css . T.pack =<< getDataFileName "ui/gui.css"
    styleContextAddProviderForScreen screen css 600

unsafeBuildObj :: (GObject o)
    => (ManagedPtr o -> o)
    -> Builder
    -> T.Text
    -> IO o
unsafeBuildObj t builder wId = builderGetObject builder wId
    >>= unsafeCastTo t . fromJust

buildOverlay :: IORef Int -> Grid -> IO Overlay
buildOverlay difficulty' b = do
    menuOverlay <- new Overlay [#expand := True]
    menuBuilder <- builderNewFromFile . T.pack
        =<< getDataFileName (T.unpack uiPath)

    menu <- builderGetObject menuBuilder "startMenu"
        >>= unsafeCastTo Popover . fromJust

    btns <- sequence $ [unsafeBuildObj Button menuBuilder] <*> [ "easy"
                                                               , "normal"
                                                               , "hard" ]

    forM_ btns \x -> do
        style <- #getStyleContext x
        #addClass style "overlayBtn"

    mapM_ noRelief btns
    mapM_ (uncurry $ setLvl difficulty' menu) $ btns `zip` [30, 50, 70]
    #addOverlay menuOverlay menu
    return menuOverlay
    where
        setLvl ref (menu :: Popover) (btn :: Button) lvl = ( on btn #clicked
            $  writeIORef ref lvl
            >> randomFill ref b
            >> #popdown menu )
            >> return ()
        noRelief (x :: Button) = set x [#relief := ReliefStyleNone]

buildGrid :: IORef (Maybe Popover) -> Int -> IO Grid
buildGrid cleanup pulse = do
    board <- new Grid [#expand := True
                      , #name := "board"
                      , #columnSpacing := 0
                      , #rowSpacing := 0]

    pass <- currentAppFontAddFile "/home/fireflower/Dev/hsudoku/ui/sudokuicons.ttf"
    when (not pass) $ putStrLn "err adding font file"

    fontMap <- carioFontMapGetDefault
    fdesc <- fontDescriptionFromString "Sudoku"

    forM_ cSpace \(x, y) -> do
        cell <- new Button [#expand := True, #relief := ReliefStyleNone]
        lbl <- getLbl x y
        style <- #getStyleContext cell
        #addClass style "normal"
        set cell [#name := lbl]

        kpBuilder <- builderNewFromFile . T.pack
            =<< getDataFileName (T.unpack uiPath)

        pad <- unsafeBuildObj Popover kpBuilder "keypad"
        kpGrid <- unsafeBuildObj Grid kpBuilder "kpGrid"
        #getChildren kpGrid >>= \xs -> forM_ xs \x' -> do
            btn <- unsafeCastTo Button x'
            set btn [#relief := ReliefStyleNone]
            pangoCtxt <- #createPangoContext btn
            #loadFont fontMap pangoCtxt fdesc

        set pad [#relativeTo := cell, #modal := False]
        on cell #clicked $ switchPopup pad cleanup
        wirePad pad cleanup board pulse
        #attach board cell (fromIntegral x) (fromIntegral y) 1 1
    return board
        where
            getLbl x y = let
                i = getBlock (y, x)
                in if odd $ i
                    then return "oddBlockCell"
                    else return "evenBlockCell"

gridArray :: Grid -> IO Board
gridArray g = do
    st <- gameState g
    return $ listArray ((1,1), (sudokuSz, sudokuSz)) st

getSltn :: Grid -> IO (Maybe [Possibility])
getSltn g = do
    arr <- gridArray g
    rand <- newStdGen
    return $ solve arr rand >>= \xs -> if null xs
        then return []
        else return $ head xs

buildControl
    :: Box
    -> Grid
    -> IORef (Maybe Popover)
    -> IORef Int
    -> Int
    -> IO Box
buildControl box board cleanup difficulty' len = do
    ctrlBox <- new Box [#expand := False
                       , #orientation := OrientationHorizontal
                       , #spacing := 2]
    #add box ctrlBox

    clrBtn <- newBtn "clear" $writeState board (cSpace `zip` repeat 0)
    redoBtn <- newBtn "new game" $ randomFill difficulty' board
    solveBtn <- newBtn "solve" $ getSltn board >>= \case
            Nothing -> flashColor len board "red"
            Just [] -> flashColor len board "green"
            Just x  -> writeState board x >> flashColor len board "green"
        >> return ()

    #add ctrlBox redoBtn
    #add ctrlBox clrBtn
    #add ctrlBox solveBtn
    return ctrlBox
    where
        newBtn lbl q = do
            btn <- new Button [#expand := True
                                 , #label := lbl
                                 , #relief := ReliefStyleNone]
            on btn #clicked $ q >> clearPopup cleanup
            return btn

buildUI :: Env -> IO Window
buildUI env = do
    win <- buildWindow (winWidth env) (winHeight env)
    applyCss win
    lastPad <- newIORef Nothing
    board <- buildGrid lastPad $ pulseDuration env
    difficulty' <- newIORef (difficulty env)
    menuOverlay <- buildOverlay difficulty' board
    #add win menuOverlay

    box <- new Box [#expand := True -- top level layout
                   , #orientation := OrientationVertical
                   , #spacing := 5 ]
    #add menuOverlay box
    #add box board

    ctrlBox <- buildControl box board lastPad difficulty' (pulseDuration env)
    #add box ctrlBox
    return win

clearPopup :: IORef (Maybe Popover) -> IO ()
clearPopup x' = do
    readIORef x' >>= \case
        Nothing -> return ()
        Just v  -> #popdown v

switchPopup :: Popover -> IORef (Maybe Popover) -> IO ()
switchPopup p x' = do
    readIORef x' >>= \case
        Nothing -> writeIORef x' (Just p) >> #popup p
        Just x | x == p -> #popdown x >> writeIORef x' Nothing
               | otherwise -> do
                    #popdown x
                    writeIORef x' $ Just p
                    #popup p

-- | Solves a blank grid and then removes cells with n - probability
randomFill :: IORef Int -> Grid -> IO ()
randomFill pRef s = do
    p <- readIORef pRef
    rand <- newStdGen
    case head <$> solve clr rand of
        Nothing -> randomFill pRef s
        Just xs | p >= maxProb -> writeState s xs
                | otherwise -> do
            g <- forM xs \x -> randomRIO (1, maxProb) >>= \p' ->
                if p' > p
                    then return x
                    else return (fst x, 0)
            writeState s g
    return ()
    where
        clr = listArray ((1,1), (sudokuSz,sudokuSz)) $ repeat 0 :: Board
        maxProb = 100



-- Set handlers for keypad and its children
wirePad :: Popover -> IORef (Maybe Popover) -> Grid -> Int -> IO ()
wirePad b p g pulse = do
    parent <- popoverGetRelativeTo b
    parent' <- unsafeCastTo Button parent
    grid <- head <$> #getChildren b >>= unsafeCastTo Grid
    children <- #getChildren grid
    forM_ children \x -> do
        x' <- unsafeCastTo Button x
        on x' #clicked do

            gv <-  toGValue (0 :: CInt)
            containerChildGetProperty g parent' "top-attach" gv
            i <- fromGValue gv :: IO CInt
            containerChildGetProperty g parent' "left-attach" gv
            j <- fromGValue gv :: IO CInt

            cellVal <- readInt . T.unpack <$> #getLabel parent' :: IO Int

            arr <- gridArray g
            putStr "detected coord: "
            print (i, j)
            cs <- conflicts arr cellVal (fromIntegral i) (fromIntegral j)
            putStrLn . unlines $ show <$> cs

            #getLabel x' >>= \case
                "\59738" -> set parent' [#label := T.empty]
                "\59737" -> if null cs
                    then flashButton parent' "red" pulse
                    else do
                        forM_ cs \(i', j') -> do
                            btn <- gridGetChildAt g (fromIntegral j') (fromIntegral i') >>= unsafeCastTo Button . fromJust
                            flashButton btn "red" pulse
                "\59735" -> do
                    set parent' [#label := T.empty]
                    getSltn g >>= \case
                        Nothing -> flashButton parent' "red" pulse
                        Just [] -> flashButton parent' "green" pulse
                        Just xs -> case lookup ((fromIntegral i), (fromIntegral j)) xs of
                            Nothing -> flashButton parent' "red" pulse
                            Just s  -> set parent' [#label := T.pack . show $ s] >> flashButton parent' "green" pulse
                v -> set parent' [#label := v]
            clearPopup p
    return ()

-- | Get state from grid to be used with Sudoku.Sudoku.solve
gameState :: Grid -> IO [Int]
gameState g = reverse <$> do
    children <- #getChildren g
    forM children \x -> do
        x' <- unsafeCastTo Button x
        #getLabel x' >>= \v -> if v == T.empty
            then return 0
            else return . read . T.unpack $ v

-- | Write xs onto g
writeState :: Grid -> [((Int, Int), Int)] -> IO ()
writeState g xs = forM_ xs \((fromIntegral -> x, fromIntegral -> y),v) -> do
    cell <- gridGetChildAt g y x >>= unsafeCastTo Button . fromJust
    set cell [#label := T.pack . f $ v]
    return ()
    where
        f 0 = []
        f x = show x

flashButton :: Button -> T.Text -> Int -> IO ()
flashButton b t pulse = do
    style <- #getStyleContext b
    #removeClass style "normal"
    #addClass style t
    forkIO do
        threadDelay pulse
        #removeClass style t
        #addClass style "normal"
    return ()


-- | Flash a supported colover over the sudoku table for len microseconds
flashColor :: Int -> Grid -> T.Text -> IO ()
flashColor len g clazz = forkIO do
    children <- #getChildren g
    forM_ children \x -> do
        x' <- unsafeCastTo Button x
        style <- #getStyleContext x'
        #removeClass style "normal"
        #addClass style clazz
    threadDelay len
    forM_ children \x -> do
        x' <- unsafeCastTo Button x
        style <- #getStyleContext x'
        #removeClass style clazz
        #addClass style "normal"
    return ()
    >> return ()

main :: IO ()
main = do
    Gtk.init Nothing
    win <- buildUI dfltEnv
    #showAll win
    Gtk.main
    `catch` (\(e::GError) -> gerrorMessage e >>= putStrLn . T.unpack)


data FcConfig = FcConfig

data PangoFontDescription = PangoFontDescription

foreign import ccall "FcConfigGetCurrent" configGetCurrent :: IO (Ptr FcConfig)

foreign import ccall "FcConfigAppFontAddFile" configAppFontAddFile :: Ptr FcConfig -> CString -> IO Bool

foreign import ccall "pango_cairo_font_map_get_default" carioFontMapGetDefault' :: IO (Ptr a)

currentAppFontAddFile :: String -> IO Bool
currentAppFontAddFile s = do
    cfg <- configGetCurrent
    str <- newCAString s
    configAppFontAddFile cfg str

carioFontMapGetDefault :: IO FontMap
carioFontMapGetDefault = do
    ptr <- carioFontMapGetDefault'
    fptr <- newForeignPtr_ ptr
    cs <- newIORef Nothing
    let mPtr = ManagedPtr fptr Nothing cs
    return $ unsafeCoerce mPtr

readInt :: String -> Int
readInt = fromMaybe 0 . readMaybe
