import XMonad
import XMonad.Util.Run
import XMonad.Hooks.ManageDocks
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Actions.WindowGo
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.XMonad
import XMonad.Actions.CycleWS
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

import XMonad.Actions.ShowText


-- mod mask key
modm = mod3Mask   	 

main :: IO ()
main = do
	xmproc <- spawnPipe "xmobar"
	xmonad $ defaultConfig {
		manageHook = manageDocks <+> manageHook defaultConfig ,
		layoutHook = avoidStruts  $  layoutHook defaultConfig ,
		-- Update Screen to Clear flashtext 
		handleEventHook = handleTimerEvent <+> handleEventHook defaultConfig ,
		-- Send to xmobar
		logHook = dynamicLogWithPP $ xmobarPP
					{ ppOutput = hPutStrLn xmproc
					, ppTitle = xmobarColor "green" "" . shorten 50
					},

		-- Border settings
		borderWidth = 3 ,
		normalBorderColor  = "#000000" ,
		focusedBorderColor = "#11ff43" , -- green

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
			[ ((modm, xK_l), flashText defaultSTConfig 1 "->" >> nextWS)
			, ((modm, xK_h), flashText defaultSTConfig 1 "<-" >> prevWS)
			, ((modm.|.shiftMask, xK_l), flashText defaultSTConfig 1 "|=>" >> shiftToNext >> nextWS)
			, ((modm.|.shiftMask, xK_h), flashText defaultSTConfig 1 "<=|" >> shiftToPrev >> prevWS)

			, ((modm, xK_9 ), sendMessage Shrink)
			, ((modm, xK_0 ), sendMessage Expand)

			, ((mod1Mask, xK_Tab ), windows W.focusDown)
			, ((mod1Mask .|. shiftMask, xK_Tab ), windows W.swapDown )

			, ((modm, xK_r ), shellPrompt  shellPromptConfig)
			, ((modm, xK_p ), shellPrompt  shellPromptConfig)
			, ((modm, xK_colon ), shellPrompt  shellPromptConfig)
			, ((modm, xK_q), spawn "killall dzen2; xmonad --recompile && xmonad --restart")

			, ((modm, xK_e), unsafeSpawn "caja ~")
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
