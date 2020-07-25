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
import XMonad.Layout.Maximize
import XMonad.Layout.NoBorders
import XMonad.Layout.BorderResize
import XMonad.Layout.ToggleLayouts
import XMonad.Layout.ResizableTile
import XMonad.Layout.SimplestFloat
import XMonad.Layout.SimpleDecoration
import XMonad.Layout.StackTile
import XMonad.Layout.ImageButtonDecoration
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
myWorkspaces = ["1", "2" ,"3", "4", "5", "6", "7", "8", "9", "10", "11", "12" ]
-- kill command
killCommand = "xmonad --recompile && xmonad --restart && xfce4-panel -r"
-- killCommand = "xmonad --restart && xfce4-panel -r"

-- layoutHook
myTall = named "Tall" $ ResizableTall 1 (3/100) (1/2) []
myStack = named "Stack" $ StackTile 2 (3/100) (5/6)
myThree = named "Three" $ ThreeColMid 1 (3/100) (1/2)
myGrid = named "Grid" $ SplitGrid XMonad.Layout.GridVariants.L 1 1 (4/5) (2/1) (3/100)
myFloat = named "Float" $ floatingDeco $ borderResize $ withBorder 4
        $ maximize $ simplestFloat
    where floatingDeco = imageButtonDeco shrinkText defaultThemeWithImageButtons
            { activeColor = "black"
            , inactiveColor = "grey"
            , fontName = "sans-serif" }
myLayout = avoidStruts $ toggleLayouts (noBorders Full) (myTall|||myStack|||myThree|||myGrid|||myFloat)

-- manageHook
myManageHook = manageDocks <+> manageHook kdeConfig <+> composeOne [
                isFullscreen -?> doFullFloat,
                isDialog -?> doFloat
             ] <+> composeAll [
--                className =? "GoldenDict" --> doFullFloat
             ]

-- handleEventHook
myHandleEventHook = handleEventHook kdeConfig
                    <+> handleTimerEvent -- Update Screen to Clear flashtext
                    <+> fullscreenEventHook

main :: IO ()
main = do
    xmproc <- spawnPipe "xmobar"
    xmonad $ kdeConfig {
        layoutHook = desktopLayoutModifiers( myLayout ),
        manageHook = myManageHook ,
        handleEventHook = myHandleEventHook ,
        -- Send to xmobar
        logHook = logHook kdeConfig
                <+> (dynamicLogWithPP $ xmobarPP
                    { ppOutput = hPutStrLn xmproc
                    , ppTitle = xmobarColor "green" "" . shorten 50 }),
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
              ((modm,                  xK_j  ), windows W.focusDown)
            , ((modm.|.shiftMask,      xK_j  ), windows W.swapDown )
            , ((modm,                  xK_k  ), windows W.focusUp  )
            , ((modm.|.shiftMask,      xK_k  ), windows W.swapUp   )
            , ((modm,                  xK_m  ), windows W.focusMaster)
            , ((modm,                  xK_Return), windows W.shiftMaster)
            , ((modm.|.shiftMask,      xK_c  ), kill)
            , ((modm,                  xK_t  ), withFocused $ windows . W.sink)
            -- window alt-tab
            , ((modm,                  xK_Tab), windows W.focusDown)
            , ((modm.|.shiftMask,      xK_Tab), windows W.focusUp  )
            , ((mod1Mask,              xK_Tab), windows W.focusDown)
            , ((mod1Mask.|.shiftMask,  xK_Tab), windows W.swapDown )
            -- window alt-ctrl-tab
            , ((mod1Mask.|.controlMask, xK_Tab), moveWsNoGreedy Next NonEmptyWS)  -- move to next WS

            -- layout toggle
            , ((modm,                  xK_space), sendMessage NextLayout)
            , ((modm.|.shiftMask,      xK_space), setLayout $ XMonad.layoutHook conf)
            , ((modm,                  xK_f  ), sendMessage ToggleLayout)

            , ((modm.|.shiftMask,      xK_n  ), refresh)

            -- workspaces
            , ((modm,                  xK_h  ), moveWsNoGreedy Prev AnyWS)
--          , ((modm,                  xK_h  ), moveWsNoGreedy Prev AnyWS >> logCurrent >>= moveFlashText)
            , ((modm,                  xK_l  ), moveWsNoGreedy Next AnyWS)
            , ((modm.|.shiftMask,      xK_h  ), shiftWsNoGreedy Prev AnyWS)
            , ((modm.|.shiftMask,      xK_l  ), shiftWsNoGreedy Next AnyWS)
            , ((modm,                  xK_n  ), moveWsNoGreedy Prev NonEmptyWS)
            , ((modm,                  xK_p  ), moveWsNoGreedy Next NonEmptyWS)
            , ((modm.|.shiftMask,      xK_n  ), shiftWsNoGreedy Prev EmptyWS)
            , ((modm.|.shiftMask,      xK_p  ), shiftWsNoGreedy Next EmptyWS)
            , ((modm,                  xK_b  ), toggleWS)
            -- physical screen
            , ((modm,                           xK_s  ), nextScreen)
            , ((modm.|.shiftMask,               xK_s  ), shiftNextScreen >> nextScreen)
--          , ((modm,                           xK_s  ), prevScreen)
            , ((modm.|.shiftMask.|.controlMask, xK_s  ), swapNextScreen)
            -- reset workspaces corresponding to physical screens
            , ((modm,                  xK_d  ), do
                screenWorkspace 1 >>= flip whenJust (windows . W.view)  -- second screen
                (windows . W.greedyView) "2"
                screenWorkspace 0 >>= flip whenJust (windows . W.view)  -- first screen
                (windows . W.greedyView) "3")

            -- shrink, expand
            , ((modm,                  xK_9  ), sendMessage Shrink)
            , ((modm,                  xK_0  ), sendMessage Expand)
            , ((modm.|.shiftMask,      xK_9  ), sendMessage MirrorExpand)
            , ((modm.|.shiftMask,      xK_0  ), sendMessage MirrorShrink)
            -- layout num
            , ((modm,                  xK_comma ), sendMessage (IncMasterN 1))
            , ((modm,                  xK_period), sendMessage (IncMasterN (-1)))
            -- split grid layout
            , ((modm,                  xK_bracketleft), sendMessage $ IncMasterCols 1)
            , ((modm,                  xK_bracketright), sendMessage $ IncMasterCols (-1))
            , ((modm.|.shiftMask,      xK_bracketleft), sendMessage $ IncMasterRows 1)
            , ((modm.|.shiftMask,      xK_bracketright), sendMessage $ IncMasterRows (-1))


            -- run application
            , ((modm.|.shiftMask,      xK_Return), spawn $ XMonad.terminal conf)
            , ((modm,                  xK_r  ), shellPrompt shellPromptConfig)
            , ((modm,                  xK_q  ), spawn killCommand)
            , ((modm,                  xK_e  ), unsafeSpawn (myFiler ++ " ~"))
            , ((modm,                  xK_o  ), unsafeSpawn myTerminal)
            , ((mod1Mask,              xK_o  ), unsafeSpawn myXmodmap)
            , ((modm.|.shiftMask,      xK_r  ), unsafeSpawn myDisp)
            , ((mod1Mask.|.shiftMask,  xK_r  ), unsafeSpawn myDisp)
            ]
            -- view mode
--          ++
--          [((m .|. modm, k), windows $ f i)
--              | (i, k) <- zip myWorkspaces [xK_F1 .. xK_F12]
--              , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
            -- greedy view mode
            ++
            [((m .|. modm, k), windows $ f i)
                | (i, k) <- zip (XMonad.workspaces conf) [xK_F1 .. xK_F12]
                , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
            --
--          ++
--          [((m .|. modm, k), screenWorkspace sc >>= flip whenJust (windows . f))
--              | (k, sc) <- zip [xK_w, xK_e, xK_r] [0..]
--              , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]



-- Mouse Binding
myMouseBindings (XConfig {XMonad.modMask = a}) = M.fromList $
            [ ((modm,                           button1), (\w -> focus w))
            , ((modm.|.shiftMask,               button1), (\w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster))
            , ((modm.|.shiftMask.|.controlMask, button1), (\w -> focus w >> mouseResizeWindow w))
            ]

-- Shell Prompt Config
shellPromptConfig = greenXPConfig {
        font = "xft:Sans-11"
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

moveWsNoGreedy d wt = do
    t <- findWorkspace getSortByIndex d wt 1
    (windows . W.view) t

shiftWsNoGreedy d wt = do
    t <- findWorkspace getSortByIndex d wt 1
    (windows . W.shift) t
    (windows . W.view) t
