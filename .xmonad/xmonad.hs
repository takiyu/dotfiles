import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import System.IO

import XMonad.Actions.WindowGo

import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Config.Desktop (desktopLayoutModifiers)
import XMonad.Layout.Named

-- mod mask key
modm = mod3Mask   	 

tall = Tall 1 (3/100) (1/2)

main = do
	xmonad $ defaultConfig{
		manageHook = manageDocks <+> manageHook defaultConfig ,
		layoutHook = avoidStruts  $  layoutHook defaultConfig ,

		-- Border settings
		borderWidth = 3 ,
		normalBorderColor  = "#000000" ,
		focusedBorderColor = "#11ff43" , -- green

		-- Set Hiragana_Katakana as mod
		modMask = modm ,

		-- use mate-terminal
		terminal = "mate-terminal"
		} 

		`additionalKeys`[
			((modm, xK_e), runOrRaise "caja" (className =? "Caja"))
-- 			((modm, xK_t), runOrRaise "urxvt" (className =? "URxvt"))
		]
