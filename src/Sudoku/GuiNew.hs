{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

module Sudoku.GuiNew where
import Control.Concurrent (forkIO, threadDelay)
import Control.Exception (catch)
import Control.Monad (forM_, when)
import Data.GI.Base
import GI.Pango.Objects.Context
import Data.GI.Base.BasicTypes
import Data.GI.Base.GError (gerrorMessage, GError(..))
import Data.GI.Base.ManagedPtr
import Data.Maybe (fromJust)
import Data.Tuple
import Foreign.C.Types
import GI.Gtk.Enums
import GI.Gtk.Objects.Box
import GI.Gtk.Objects.Builder
import GI.Gtk.Objects.Button
import GI.Gtk.Objects.Container
import GI.Gtk.Objects.CssProvider
import GI.Gtk.Objects.Grid
import GI.Gtk.Objects.Overlay
import GI.Gtk.Objects.Popover
import GI.Gtk.Objects.StyleContext
import GI.Gtk.Objects.Window
import GI.Pango.Objects.FontMap
import GI.Pango.Structs.FontDescription
import Paths_sudoku
import Sudoku.Internal.Bindings
import Sudoku.Internal.Bridge
import Sudoku.Internal.Internal
import Sudoku.Internal.Sudoku
import qualified Data.Text as T
import qualified GI.Gtk.Functions as Gtk (main, init, mainQuit)

data Env = Env { difficulty :: Int
               , pulse      :: Int }

data App = App { keypad      :: Popover
               , board       :: Grid
               , window      :: Window }


type Font = (FontMap, FontDescription)

cssPath :: T.Text
cssPath = "ui/gui.css"

uiPath :: T.Text
uiPath = "ui/app.ui"

dfltEnv :: Env
dfltEnv = Env 50 350000

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

-- Build UI --------------------------------------------------------------------

buildFont :: IO Font
buildFont = do
    pass <- currentAppFontAddFile "/home/fireflower/Dev/hsudoku/ui/sudokuicons.ttf"
    when (not pass) $ error "err adding font file"
    fontMap <- carioFontMapGetDefault
    fdesc <- fontDescriptionFromString "Sudoku"
    return (fontMap, fdesc)

addBtnFont :: Font -> Button -> IO ()
addBtnFont fnt btn = #createPangoContext btn >>= \x -> do
    #loadFont (fst fnt) x $ snd fnt
    return ()

buildBoard :: Builder -> IO Grid
buildBoard b = do
    kp <- unsafeBuildObj Popover b "keypad"
    board' <- unsafeBuildObj Grid b "board"
    forM_ cSpace \(i, j) -> do
        cellT <- getLbl i j
        cell <- new Button [ #expand := True
                           , #relief := ReliefStyleNone
                           , #name   := "cell" ]
        on cell #clicked $ cellBtn kp cell
        style <- #getStyleContext cell
        #addClass style cellT
        #attach board' cell (fromIntegral i) (fromIntegral j) 1 1
    return board'
        where
            getLbl x y = let
                i = getBlock (y, x)
                in if odd $ i
                    then return "oddCell"
                    else return "evenCell"

buildOverlayLayout :: Grid -> Builder -> IO ()
buildOverlayLayout g b = do
    overlay <- unsafeBuildObj Overlay b "init_overlay"
    diffBox <- unsafeBuildObj Box b "diff_layout"
    #addOverlay overlay diffBox
    difficultyBtns b g
    #show overlay

buildCtrlLayout :: Int -> Grid -> Builder -> IO ()
buildCtrlLayout pulse' g b = do
    newGame_ <- unsafeBuildObj Button b "new_game_btn"
    check_ <- unsafeBuildObj Button b "check_btn"
    solve_ <- unsafeBuildObj Button b "solve_btn"
    on newGame_ #clicked $ newGameBtn b
    on solve_ #clicked $ solveBtn pulse' b g
    on check_ #clicked $ checkBtn pulse' g
    return ()

buildKeypad :: Font -> Int -> Grid -> Builder -> IO Popover
buildKeypad (map, desc) pulse' g b = do
    kp <- unsafeBuildObj Popover b "keypad"
    let prefix = "kp_"
    forM_ [1..sudokuSz] \x -> do
        btn <- unsafeBuildObj Button b . T.pack $ prefix <> show x
        on btn #clicked $ kpDigitBtn kp x
    checkBtn' <- unsafeBuildObj Button b "kp_check"
    clearBtn <- unsafeBuildObj Button b "kp_clear"
    cheatBtn <- unsafeBuildObj Button b "kp_cheat"
    on checkBtn' #clicked $ kpCheckBtn pulse' kp g
    on clearBtn #clicked $ kpClearBtn kp
    on cheatBtn #clicked $ kpCheatBtn pulse' kp g
    return kp

buildWindow :: Builder -> IO Window
buildWindow b = do
    win <- unsafeBuildObj Window b "main"
    on win #destroy Gtk.mainQuit
    return win

buildApp :: Env -> IO App
buildApp e = do
    builder <- builderNewFromFile . T.pack
        =<< getDataFileName (T.unpack uiPath)
    board' <- buildBoard builder
    buildOverlayLayout board' builder
    buildCtrlLayout (pulse e) board' builder
    kpFont <- buildFont
    kp <- buildKeypad kpFont (pulse e) board' builder
    win <- buildWindow builder
    applyCss win
    return $ App kp board' win

-- Button Controls -------------------------------------------------------------

cellBtn :: Popover -> Button -> IO ()
cellBtn kp btn = do
    #setRelativeTo kp $ Just btn
    #popup kp

kpDigitBtn :: Popover -> Int -> IO ()
kpDigitBtn p v = do
    parent <- #getRelativeTo p >>= unsafeCastTo Button
    set parent [ #label := T.pack . show $ v ]
    #popdown p

kpCheckBtn :: Int -> Popover -> Grid -> IO ()
kpCheckBtn pulse' p g = do
    cell <- keypadCell g p
    gridConflicts g cell >>= \case
        [] -> flashColor pulse' g [(fst cell)] Green
        xs -> flashColor pulse' g xs Red
    #popdown p
    return ()

kpClearBtn :: Popover -> IO ()
kpClearBtn p = do
    parent <- #getRelativeTo p >>= unsafeCastTo Button
    set parent [#label := T.empty]
    #popdown p

kpCheatBtn :: Int -> Popover -> Grid -> IO ()
kpCheatBtn pulse' p g = do
    kpClearBtn p
    sltn <- gridSolve g
    cord <-fst <$> keypadCell g p
    case sltn of
        Nothing -> flashColor pulse' g cSpace Red
        Just x -> case lookup cord x of
            Nothing -> flashColor pulse' g [cord] Green
            Just x' -> gridUpdate g [(cord, x')]
                >> flashColor pulse' g [cord] Green
    #popdown p

solveBtn :: Int -> Builder -> Grid -> IO ()
solveBtn pulse b g = do
    btn <- unsafeBuildObj Button b "solve_btn"
    on btn #clicked $ do
        gridSolve g >>= \case
            Nothing -> flashColor pulse g cSpace Red
            Just [] -> flashColor pulse g cSpace Green
            Just xs -> gridUpdate g xs
    return ()

checkBtn :: Int -> Grid -> IO ()
checkBtn pulse g = do
    board' <- gridArray g
    case allConflicts board' of
        [] -> flashColor pulse g cSpace Green
        xs -> flashColor pulse g xs Red

difficultyBtns :: Builder -> Grid -> IO ()
difficultyBtns b g = do
    menuBox <- unsafeBuildObj Box b "diff_layout"
    easyBtn <- unsafeBuildObj Button b "easy_btn"
    normalBtn <- unsafeBuildObj Button b "normal_btn"
    hardBtn <- unsafeBuildObj Button b "hard_btn"
    on easyBtn #clicked do
        #hide menuBox
        gridRegen g 30
    on normalBtn #clicked do
        #hide menuBox
        gridRegen g 50
    on hardBtn #clicked do
        #hide menuBox
        gridRegen g 70
    return ()

newGameBtn :: Builder -> IO ()
newGameBtn b = do
    menuBox <- unsafeBuildObj Box b "diff_layout"
    #show menuBox

--------------------------------------------------------------------------------

data Color = Red | Green -- colors must be supported in CSS

traverseTable :: Grid ->  [Point] -> (Button -> IO ()) -> IO ()
traverseTable g ps f = do
    forM_ ps \(fromIntegral -> i, fromIntegral -> j) -> do
        cell <- #getChildAt g j i >>= unsafeCastTo Button . fromJust
        f cell

flashColor :: Int -> Grid -> [Point] -> Color -> IO ()
flashColor pulse' g ps c = traverseTable g ps \x -> do
    style <- #getStyleContext x
    #addClass style $ sel c
    forkIO $ threadDelay pulse' >> #removeClass style (sel c)
    return ()
    where
        sel = \case
            Red -> "red"
            Green -> "green"

main :: IO ()
main = do
    Gtk.init Nothing
    app <- buildApp dfltEnv
    #showAll $ window app
    Gtk.main
    `catch` (\(e::GError) -> gerrorMessage e >>= putStrLn . T.unpack)
