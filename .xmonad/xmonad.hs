import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.ShowText
import XMonad.Actions.WindowGo
import XMonad.Config.Gnome
import XMonad.Util.Run
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Util.Loggers (logCurrent)
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Layout
import XMonad.Layout.Gaps
import XMonad.Layout.ResizableTile
import XMonad.Layout.NoBorders
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import Data.Maybe (fromMaybe)
-- import XMonad.Layout.SimpleFloat
-- import XMonad.Layout.Maximize
import XMonad.Hooks.ManageHelpers

import qualified XMonad.StackSet as W
import qualified Data.Map        as M
import qualified XMonad.Actions.FlexibleManipulate as Flex


-- my apps
myFiler = "nemo"
myTerminal = "gnome-terminal"
-- mod mask key
modm = mod3Mask   	 

-- layoutHook
tall = ResizableTall 1 (3/100) (1/2) []
myLayout = avoidStruts $ smartBorders $ mkToggle (single FULL) (tall ||| Mirror tall)

-- manageHook
myManageHook = manageDocks <+> manageHook gnomeConfig <+> composeOne [
				 isFullscreen -?> doFullFloat
			 ]

-- handleEventHook
myHandleEventHook = handleTimerEvent -- Update Screen to Clear flashtext 
					<+> handleEventHook gnomeConfig

main :: IO ()
main = do
	xmproc <- spawnPipe "xmobar"
	xmonad $ gnomeConfig {
		layoutHook = myLayout ,
		manageHook = myManageHook ,
		handleEventHook = myHandleEventHook ,
		-- Send to xmobar
		logHook = logHook gnomeConfig <+> (dynamicLogWithPP $ xmobarPP
				{ ppOutput = hPutStrLn xmproc
				, ppTitle = xmobarColor "green" "" . shorten 50 }) ,

		workspaces = ["1", "2" ,"3", "4", "5", "6", "7", "8", "9"] ,
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
tmpKeys x = foldr M.delete (keys defaultConfig x) (keysToDel x)
myKeys x = keysToAdd x `M.union` tmpKeys x
-- Keys To Delete
keysToDel :: XConfig Layout -> [(KeyMask, KeySym)]
keysToDel x =
			[ (modm              , xK_p )
			, (modm              , xK_q )
			, (modm .|. shiftMask, xK_q )
			]
			++
			[ (modm, k) | k <- [xK_1 .. xK_9]]
			++
			[ (modm .|. shiftMask, k) | k <- [xK_1 .. xK_9]]

-- Keys To Add
keysToAdd conf@(XConfig {modMask = a}) = M.fromList
			[ ((modm, xK_h), prevWS >> logCurrent >>= moveFlashText)
			, ((modm, xK_l), nextWS >> logCurrent >>= moveFlashText)
			, ((modm.|.shiftMask, xK_h), shiftToPrev >> prevWS >> logCurrent >>= shiftLeftFlashText)
			, ((modm.|.shiftMask, xK_l), shiftToNext >> nextWS >> logCurrent >>= shiftRightFlashText)

			-- tall
			, ((modm, xK_f ), sendMessage (Toggle FULL))
			, ((modm, xK_9 ), sendMessage Shrink)
			, ((modm, xK_0 ), sendMessage Expand)
			, ((modm.|.shiftMask, xK_9 ), sendMessage MirrorExpand)
			, ((modm.|.shiftMask, xK_0 ), sendMessage MirrorShrink)

			-- alt tab
			, ((mod1Mask, xK_Tab ), windows W.focusDown)
			, ((mod1Mask .|. shiftMask, xK_Tab ), windows W.swapDown )

			, ((modm, xK_r ), shellPrompt  shellPromptConfig)
			, ((modm, xK_q ), spawn "killall dzen2; xmonad --recompile && xmonad --restart")

			, ((modm, xK_e ), unsafeSpawn (myFiler ++ " ~"))
			, ((modm, xK_o ), unsafeSpawn myTerminal)
			, ((mod1Mask, xK_o ), unsafeSpawn myTerminal)

			, ((modm, xK_F5), refresh)
			]

-- Mouse Binding
myMouseBindings (XConfig {XMonad.modMask = a}) = M.fromList $
			[ ((modm,button1), (\w -> focus w >> mouseMoveWindow w
								>> windows W.shiftMaster))
			, ((modm.|.shiftMask, button1), (\w -> focus w 
									>> Flex.mouseWindow Flex.resize w))
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
mySTConfig = defaultSTConfig{ st_font = "xft:Droid Sans:pixelsize=40"
							, st_bg   = "black"
							, st_fg   = "green"
							}
moveFlashText m = flashText mySTConfig 1 (" " ++ fromMaybe "" m ++ " ")
shiftRightFlashText m = flashText mySTConfig 1 ("->" ++ fromMaybe "" m ++ "")
shiftLeftFlashText  m = flashText mySTConfig 1 ("" ++ fromMaybe "" m ++ "<-")

