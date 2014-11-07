import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.ShowText
import XMonad.Actions.WindowGo
import XMonad.Util.Run
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Util.Loggers (logCurrent)
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.EwmhDesktops
import XMonad.Layout
import XMonad.Layout.Gaps
import XMonad.Layout.ResizableTile
import XMonad.Layout.NoBorders
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import Data.Maybe (fromMaybe)
import XMonad.Layout.SimpleFloat

import qualified XMonad.StackSet as W
import qualified Data.Map        as M


-- mod mask key
modm = mod3Mask   	 

-- StartUp
startup :: X ()
startup = do
          spawn "xmodmap /home/takiyu/.xmodmaprc"
          spawn "mate-terminal"

-- layout
tall = ResizableTall 1 (3/100) (1/2) []
-- myLayout = avoidStruts $ smartBorders $ mkToggle (single FULL) (tall ||| Mirror tall ||| simpleFloat)
myLayout = avoidStruts $ smartBorders $ mkToggle (single FULL) (tall ||| Mirror tall)
-- handleEventHook
myHandleEventHook = fullscreenEventHook -- for ewmh
					<+> handleTimerEvent -- Update Screen to Clear flashtext 
					<+> handleEventHook defaultConfig

main :: IO ()
main = do
	xmproc <- spawnPipe "xmobar"
	xmonad $ ewmh defaultConfig {
		layoutHook = myLayout ,
		manageHook = manageDocks <+> manageHook defaultConfig ,
		handleEventHook = myHandleEventHook,
		-- Send to xmobar
		logHook = dynamicLogWithPP $ xmobarPP
					{ ppOutput = hPutStrLn xmproc
					, ppTitle = xmobarColor "green" "" . shorten 50
					},
		-- Start Up
		startupHook = startup,

		-- Border settings
		borderWidth = 3 ,
		normalBorderColor  = "#6666aa" ,
		focusedBorderColor = "#ED8931" ,

		-- Set Hiragana_Katakana as mod
		modMask = mod3Mask ,

		-- Add New KeyBinds
		keys = newKeys,

		-- Use mate-terminal
		terminal = "mate-terminal" 
		}


-- Make New Key Binding
tmpKeys x = foldr M.delete (keys defaultConfig x) (keysToDel x)
newKeys x = keysToAdd x `M.union` tmpKeys x
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

			, ((modm, xK_e ), unsafeSpawn "caja ~")
			, ((modm, xK_o ), unsafeSpawn "mate-terminal")
			, ((mod1Mask, xK_q ), unsafeSpawn "xmodmap ~/.xmodmaprc")

			, ((modm, xK_F5), refresh)
			]

-- Shell Prompt Config
shellPromptConfig = defaultXPConfig { 
		font = "xft:Sans-9:bold"
		, bgColor  = "black"
		, fgColor  = "grey"
		, bgHLight = "#000000"
		, fgHLight = "#FF0000"
		, position = Bottom
    }

-- flashtext settings
mySTConfig = defaultSTConfig{ st_font = "xft:Droid Sans:pixelsize=40"
							, st_bg   = "black"
							, st_fg   = "green"
							}
moveFlashText m = flashText mySTConfig 1 (" " ++ fromMaybe "" m ++ " ")
shiftRightFlashText m = flashText mySTConfig 1 ("->" ++ fromMaybe "" m ++ "")
shiftLeftFlashText  m = flashText mySTConfig 1 ("" ++ fromMaybe "" m ++ "<-")

