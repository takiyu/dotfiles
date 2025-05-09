import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.ShowText
import XMonad.Actions.WindowGo
import XMonad.Config.Kde
import XMonad.Config.Desktop (desktopLayoutModifiers)
import XMonad.Util.Loggers (logCurrent)
import XMonad.Util.Run
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Util.WorkspaceCompare
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Layout
import XMonad.Layout.Named
import XMonad.Layout.NoBorders
import XMonad.Layout.BorderResize
import XMonad.Layout.ToggleLayouts
import XMonad.Layout.ResizableTile
import XMonad.Layout.SimpleDecoration
import XMonad.Layout.StackTile
import XMonad.Layout.ThreeColumns
import XMonad.Layout.GridVariants
import Data.Maybe (fromMaybe)

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

-- my apps
myFiler = "thunar"
myTerminal = "xfce4-terminal"
myXmodmap = "load_xmodmap.sh"
myDisp = "rot-xrandr.sh"
-- mod mask key
modm = mod3Mask
-- workspaces
myWorkspaces = ["1", "2" ,"3", "4", "5", "6", "7", "8", "9", "10", "11", "12"]
-- kill command
killCommand = "xmonad --recompile && xmonad --restart && xfce4-panel -r"

-- layoutHook
myTall = named "Tall" $ ResizableTall 1 (3/100) (1/2) []
myStack = named "Stack" $ StackTile 2 (3/100) (5/6)
myThree = named "Three" $ ThreeColMid 1 (3/100) (1/2)
myGrid = named "Grid" $ SplitGrid XMonad.Layout.GridVariants.L 1 1 (4/5) (2/1) (3/100)
myLayout = avoidStruts $ toggleLayouts (noBorders Full)
                                       (myTall|||myStack|||myThree|||myGrid)

-- manageHook
myManageHook = manageDocks <+> manageHook kdeConfig <+> composeOne [
        isFullscreen -?> doFullFloat,
        isDialog -?> doFloat
    ] <+> composeAll [
--      className =? "GoldenDict" --> doFullFloat
    ]

-- handleEventHook
myHandleEventHook = handleEventHook kdeConfig
                    <+> handleTimerEvent -- Update Screen to Clear flashtext
                    <+> fullscreenEventHook

main :: IO ()
main = do
--     xmproc <- spawnPipe "xmobar"
    xmonad $ kdeConfig {
        layoutHook = desktopLayoutModifiers( myLayout ),
        manageHook = myManageHook ,
        handleEventHook = myHandleEventHook ,
        -- Send to xmobar
--         logHook = logHook kdeConfig
--                 <+> (dynamicLogWithPP $ xmobarPP
--                     { ppOutput = hPutStrLn xmproc
--                     , ppTitle = xmobarColor "green" "" . shorten 50 }),
        -- Workspaces
        workspaces =  myWorkspaces,
        -- Border settings
        borderWidth = 3 ,
        normalBorderColor  = "#555577" ,
        focusedBorderColor = "red" ,
        -- Set mod key
        modMask = modm ,
        -- Set Binds
        keys = myKeyBindings,
        mouseBindings = myMouseBindings,
        -- Set terminal
        terminal = myTerminal
    }

-- Make New Key Binding
myKeyBindings :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeyBindings conf@(XConfig {XMonad.modMask = a}) = M.fromList $
    [
    -- window
      ((modm,                xK_j     ), windows W.focusDown)
    , ((modm.|.shiftMask,    xK_j     ), windows W.swapDown )
    , ((modm,                xK_k     ), windows W.focusUp  )
    , ((modm.|.shiftMask,    xK_k     ), windows W.swapUp   )
    , ((modm,                xK_m     ), windows W.focusMaster)
    , ((modm,                xK_Return), windows W.shiftMaster)
    , ((modm.|.shiftMask,    xK_c     ), kill)
    , ((modm.|.shiftMask,    xK_v     ), kill)
    , ((modm,                xK_t     ), withFocused $ windows . W.sink)
    -- window alt-tab
--  , ((modm,                xK_Tab   ), nextScreen)
--  , ((modm.|.shiftMask,    xK_Tab   ), prevScreen)
--  , ((mod1Mask,            xK_Tab   ), nextScreen)
--  , ((mod1Mask.|.shiftMask,   xK_Tab), prevScreen)
    -- window alt-ctrl-tab (move to next WS)
--  , ((mod1Mask.|.controlMask, xK_Tab), toggleWS)

    -- layout toggle
    , ((modm,                xK_space ), sendMessage NextLayout)
    , ((modm.|.shiftMask,    xK_space ), setLayout $ XMonad.layoutHook conf)
    , ((modm,                xK_f     ), sendMessage ToggleLayout)

    -- workspaces
    , ((modm,                xK_h     ), moveWs Prev AnyWS)
--  , ((modm,                xK_h     ), moveWs Prev AnyWS >> logCurrent >>= moveFlashText)
    , ((modm,                xK_l     ), moveWs Next AnyWS)
    , ((modm.|.shiftMask,    xK_h     ), shiftWs Prev AnyWS)
    , ((modm.|.shiftMask,    xK_l     ), shiftWs Next AnyWS)
    , ((modm,                xK_n     ), moveWs Prev NonEmptyWS)
    , ((modm,                xK_p     ), moveWs Next NonEmptyWS)
    , ((modm.|.shiftMask,    xK_n     ), shiftWs Prev EmptyWS)
    , ((modm.|.shiftMask,    xK_p     ), shiftWs Next EmptyWS)
    , ((modm,                xK_b     ), toggleWS)
    -- physical screen
    , ((modm,                xK_s     ), nextScreen)
    , ((modm.|.shiftMask,    xK_s     ), shiftNextScreen >> nextScreen)
--  , ((modm,                xK_s     ), prevScreen)
    , ((modm.|.controlMask,  xK_s     ), swapNextScreen)
    -- reset workspaces corresponding to physical screens
    , ((modm,                 xK_d    ), do
            screenWorkspace 2 >>= flip whenJust (windows . W.view)
            (windows . W.greedyView) "11"   -- third screen
            screenWorkspace 1 >>= flip whenJust (windows . W.view)
            (windows . W.greedyView) "5"   -- second screen
            screenWorkspace 0 >>= flip whenJust (windows . W.view)
            (windows . W.greedyView) "2")  -- first screen

    -- shrink, expand
    , ((modm,                xK_9     ), sendMessage Shrink)
    , ((modm,                xK_0     ), sendMessage Expand)
    , ((modm.|.shiftMask,    xK_9     ), sendMessage MirrorExpand)
    , ((modm.|.shiftMask,    xK_0     ), sendMessage MirrorShrink)
    -- layout numbers
    , ((modm,                xK_comma ), sendMessage (IncMasterN 1))
    , ((modm,                xK_period), sendMessage (IncMasterN (-1)))
    -- split grid layout
    , ((modm,                xK_bracketleft ), sendMessage $ IncMasterCols 1)
    , ((modm,                xK_bracketright), sendMessage $ IncMasterCols (-1))
    , ((modm.|.shiftMask,    xK_bracketleft ), sendMessage $ IncMasterRows 1)
    , ((modm.|.shiftMask,    xK_bracketright), sendMessage $ IncMasterRows (-1))

    -- run application
    , ((modm.|.shiftMask,    xK_Return), spawn $ XMonad.terminal conf)
    , ((modm,                xK_r     ), shellPrompt shellPromptConfig)
    , ((modm.|.shiftMask,    xK_q     ), spawn killCommand)
    , ((modm,                xK_e     ), unsafeSpawn (myFiler ++ " ~"))
    , ((modm,                xK_o     ), unsafeSpawn myTerminal)
    , ((mod1Mask,            xK_o     ), unsafeSpawn myXmodmap)
    , ((modm.|.shiftMask,    xK_r     ), unsafeSpawn myDisp)
    , ((mod1Mask.|.shiftMask, xK_r    ), unsafeSpawn myDisp)
    ]
    -- view mode
--  ++
--  [((m .|. modm, k), windows $ f i)
--          | (i, k) <- zip myWorkspaces [xK_F1 .. xK_F12]
--          , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
    -- greedy view mode
    ++
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_F1 .. xK_F12]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    --
--  ++
--  [((m .|. modm, k), screenWorkspace sc >>= flip whenJust (windows . f))
--          | (k, sc) <- zip [xK_w, xK_e, xK_r] [0..]
--          , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

-- Mouse Binding
myMouseBindings (XConfig {XMonad.modMask = a}) = M.fromList $
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster))
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster))
    , ((modm,             button4), (\w -> moveWs Prev NonEmptyWS))
    , ((modm,             button5), (\w -> moveWs Next NonEmptyWS))
    , ((modm.|.shiftMask, button4), (\w -> shiftWs Prev AnyWS))
    , ((modm.|.shiftMask, button5), (\w -> shiftWs Next AnyWS))
    ]

-- Shell Prompt Config
shellPromptConfig = greenXPConfig {
    font = "xft:Sans-12"
    , bgColor  = "black"
    , fgColor  = "white"
    , bgHLight = "#000000"
    , fgHLight = "#FF0000"
    , borderColor = "#000000"
    , position = Top
}

-- Flashtext settings
-- moveFlashText m = flashText mySTConfig 1 (" " ++ fromMaybe "" m ++ " ")
-- shiftRightFlashText m = flashText mySTConfig 1 ("->" ++ fromMaybe "" m ++ "")
-- shiftLeftFlashText  m = flashText mySTConfig 1 ("" ++ fromMaybe "" m ++ "<-")
-- debugFlashText m = flashText mySTConfig 1 ("Debug: " ++ m)
-- mySTConfig = defaultSTConfig{ st_font = "xft:Droid Sans:pixelsize=40"
--                             , st_bg   = "black"
--                             , st_fg   = "green"
--                             }

-- Moving alias
moveWs d wt = do
    -- Greedy Moving
    -- moveTo d wt
    -- Non-Greedy Moving
    t <- findWorkspace getSortByIndex d wt 1
    (windows . W.view) t
-- Shifting alias
shiftWs d wt = do
    -- Greedy Shifting
    -- shiftTo d wt
    -- moveTo d wt
    -- Non-Greedy Shifting
    t <- findWorkspace getSortByIndex d wt 1
    (windows . W.shift) t
    (windows . W.view) t
