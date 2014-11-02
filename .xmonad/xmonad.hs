import XMonad
import XMonad.Util.Run
import XMonad.Hooks.ManageDocks
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Actions.WindowGo

import qualified Data.Map as M
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.XMonad
import XMonad.Actions.CycleWS


-- mod mask key
modm = mod3Mask   	 

main :: IO ()
main = do
	xmonad $ defaultConfig{
		manageHook = manageDocks <+> manageHook defaultConfig ,
		layoutHook = avoidStruts  $  layoutHook defaultConfig ,

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

		`additionalKeys`[
-- 			((modm, xK_e), runOrRaise "caja" (className =? "Caja"))
-- 			((modm, xK_t), runOrRaise "urxvt" (className =? "URxvt"))
		]

-- Make New Key Binding
tmpKeys x = foldr M.delete (keys defaultConfig x) (keysToDel x)
newKeys x = keysToAdd x `M.union` tmpKeys x
-- Keys To Delete
keysToDel :: XConfig Layout -> [(KeyMask, KeySym)]
keysToDel x =
			[ (modm              , xK_p )
			, (modm .|. shiftMask, xK_q )
			]++
			[ (modm, k) | k <- [xK_1 .. xK_9]]
-- Keys To Add
keysToAdd conf@(XConfig {modMask = a}) = M.fromList
			[ ((modm, xK_c), kill)
			, ((modm, xK_l), nextWS)
			, ((modm, xK_h), prevWS)
			, ((modm .|. shiftMask, xK_l), shiftToNext >> nextWS)
			, ((modm .|. shiftMask, xK_h),   shiftToPrev >> prevWS)
			, ((modm, xK_9 ), sendMessage Shrink)
			, ((modm, xK_0 ), sendMessage Expand)
			, ((modm, xK_r ), shellPrompt  defaultXPConfig)
			, ((modm, xK_p ), shellPrompt  defaultXPConfig)
			, ((modm, xK_q), spawn "killall dzen2; xmonad --recompile && xmonad --restart")
			]
