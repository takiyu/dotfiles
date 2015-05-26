import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.ShowText
import XMonad.Actions.WindowGo
import XMonad.Config.Gnome
import XMonad.Config.Desktop (desktopLayoutModifiers)
import XMonad.Util.Run
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Util.Loggers (logCurrent)
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
import XMonad.Layout.Spiral
import XMonad.Layout.SimplestFloat
import XMonad.Layout.SimpleDecoration
import XMonad.Layout.ButtonDecoration
import XMonad.Layout.ImageButtonDecoration
import XMonad.Layout.DecorationAddons
import Data.Maybe (fromMaybe)
import XMonad.Hooks.ICCCMFocus -- for android studio

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

import XMonad.Layout.StackTile
import XMonad.Actions.PhysicalScreens


-- my apps
myFiler = "nemo"
myTerminal = "gnome-terminal"
myXmodmap = "xmodmap ~/.Xmodmap"
-- mod mask key
modm = mod3Mask   	 
-- workspaces
myWorkspaces = ["a", "b" ,"c", "d", "e", "f", "g", "h", "i", "j", "k", "l" ]
-- myWorkspaces = withScreens 2 ["1", "2", "3", "4", "5", "6", "7", "8", "9"]

-- layoutHook
myTall = named "Tall" $ ResizableTall 1 (3/100) (1/2) []
myStack = StackTile 2 (3/100) (5/6)
-- myGrid = named "Grid" $ GridRatio (4/3)
myFloat = named "Float" $ floatingDeco $ borderResize $ withBorder 4
		$ maximize $ simplestFloat
-- 	where floatingDeco l = buttonDeco shrinkText defaultThemeWithButtons l
	where floatingDeco = imageButtonDeco shrinkText defaultThemeWithImageButtons
		{ activeColor = "black"
		, inactiveColor = "grey"
		, fontName = "sans-serif" }
myLayout = avoidStruts $ toggleLayouts (noBorders Full) (myTall|||myStack|||myFloat)

-- manageHook
myManageHook = manageDocks <+> manageHook gnomeConfig <+> composeOne [
				isFullscreen -?> doFullFloat,
				isDialog -?> doFloat
			 ] <+> composeAll [
-- 				className =? "GoldenDict" --> doFullFloat
			 ]

-- handleEventHook
myHandleEventHook = handleTimerEvent -- Update Screen to Clear flashtext 
					<+> handleEventHook gnomeConfig
					<+> fullscreenEventHook

main :: IO ()
main = do
	xmproc <- spawnPipe "xmobar"
	xmonad $ gnomeConfig {
		layoutHook = desktopLayoutModifiers( myLayout ),
		manageHook = myManageHook ,
		handleEventHook = myHandleEventHook ,
		-- Send to xmobar
		logHook = logHook gnomeConfig 
				<+> (dynamicLogWithPP $ xmobarPP
					{ ppOutput = hPutStrLn xmproc
					, ppTitle = xmobarColor "green" "" . shorten 50 })
				<+> takeTopFocus , -- for android studio(Java)

		-- Workspaces
		workspaces =  myWorkspaces,
		-- Border settings
		borderWidth = 3 ,
		normalBorderColor  = "#555577" ,
		focusedBorderColor = "red" ,

		-- Set Hiragana_Katakana as mod
		modMask = mod3Mask ,

		-- Add New KeyBinds
		keys = myKeys,

		-- Mouse Binding
		mouseBindings = myMouseBindings,

		-- set terminal
		terminal = myTerminal
	}


-- Make New Key Binding
myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
			[
			  ((modMask .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)
			, ((modMask .|. shiftMask, xK_c     ), kill)
			, ((modMask,               xK_Return), windows W.swapMaster)
			, ((modMask .|. shiftMask, xK_j     ), windows W.swapDown  )
			, ((modMask .|. shiftMask, xK_k     ), windows W.swapUp    )
			-- layout toggle
			, ((modMask,               xK_space ), sendMessage NextLayout)
			, ((modMask .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)
			, ((modm, xK_f ), sendMessage ToggleLayout)

			, ((modMask,               xK_n     ), refresh)

			-- alt tab
			, ((modMask,               xK_Tab   ), windows W.focusDown)
			, ((modMask .|. shiftMask, xK_Tab   ), windows W.focusUp  )
			, ((mod1Mask,              xK_Tab   ), windows W.focusDown)
			, ((mod1Mask .|. shiftMask, xK_Tab  ), windows W.swapDown )
			, ((modMask,               xK_j     ), windows W.focusDown)
			, ((modMask,               xK_k     ), windows W.focusUp  )
			, ((modMask,               xK_m     ), windows W.focusMaster  )
			-- workspaces
   			, ((modm, xK_h),             prevWS >> logCurrent >>= moveFlashText)
			, ((modm, xK_l),             nextWS >> logCurrent >>= moveFlashText)
			, ((modm.|.shiftMask, xK_h), shiftToPrev >> prevWS >> logCurrent >>= shiftLeftFlashText)
			, ((modm.|.shiftMask, xK_l), shiftToNext >> nextWS >> logCurrent >>= shiftRightFlashText)
			-- physical screen
			, ((modm, xK_2), onPrevNeighbour W.view >> logCurrent >>= moveFlashText)
			, ((modm, xK_3), onNextNeighbour W.view >> logCurrent >>= moveFlashText)
			, ((modm.|.shiftMask,   xK_2), onPrevNeighbour W.shift >> logCurrent >>= shiftLeftFlashText)
			, ((modm.|.controlMask, xK_2), onPrevNeighbour W.shift >> logCurrent >>= shiftLeftFlashText)
			, ((modm.|.shiftMask,   xK_3), onNextNeighbour W.shift >> logCurrent >>= shiftRightFlashText)
			, ((modm.|.controlMask, xK_3), onNextNeighbour W.shift >> logCurrent >>= shiftRightFlashText)


			-- Shrink, Expand
			, ((modm, xK_9 ), sendMessage Shrink)
			, ((modm, xK_0 ), sendMessage Expand)
			, ((modm.|.shiftMask, xK_9 ), sendMessage MirrorExpand)
			, ((modm.|.shiftMask, xK_0 ), sendMessage MirrorShrink)

			, ((modMask,               xK_t     ), withFocused $ windows . W.sink)

			, ((modMask              , xK_comma ), sendMessage (IncMasterN 1))
			, ((modMask              , xK_period), sendMessage (IncMasterN (-1)))

			-- run application
			, ((modm, xK_r ), shellPrompt  shellPromptConfig)
			, ((modm, xK_q ), spawn "killall dzen2; xmonad --recompile && xmonad --restart")
			, ((modm, xK_e ), unsafeSpawn (myFiler ++ " ~"))
			, ((modm, xK_o ), unsafeSpawn myTerminal)
			, ((mod1Mask, xK_o ), unsafeSpawn myXmodmap)
			]
-- 			++
-- 			[((m .|. modMask, k), windows $ f i)
-- 				| (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
-- 				, (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
-- 			++
-- 			[((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
-- 				| (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
-- 				, (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]



-- Mouse Binding
myMouseBindings (XConfig {XMonad.modMask = a}) = M.fromList $
			[ ((modm,button1), (\w -> focus w))
			, ((modm.|.shiftMask, button1), (\w -> focus w >> mouseMoveWindow w 
														   >> windows W.shiftMaster))
			, ((modm.|.shiftMask.|.controlMask, button1), (\w -> focus w >> mouseResizeWindow w))
			]

-- Shell Prompt Config
shellPromptConfig = defaultXPConfig { 
		font = "xft:Sans-11"
		, bgColor  = "black"
		, fgColor  = "white"
		, bgHLight = "#000000"
		, fgHLight = "#FF0000"
		, borderColor = "#000000"
		, position = Top
    }

-- flashtext settings
moveFlashText m = flashText mySTConfig 1 (" " ++ fromMaybe "" m ++ " ")
shiftRightFlashText m = flashText mySTConfig 1 ("->" ++ fromMaybe "" m ++ "")
shiftLeftFlashText  m = flashText mySTConfig 1 ("" ++ fromMaybe "" m ++ "<-")
mySTConfig = defaultSTConfig{ st_font = "xft:Droid Sans:pixelsize=40"
							, st_bg   = "black"
							, st_fg   = "green"
							}

