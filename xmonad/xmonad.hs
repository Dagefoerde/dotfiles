{-# LANGUAGE FlexibleContexts #-}

import XMonad
import XMonad.Config.Mate
import qualified Data.Map as M

-- For xmonad-log-applet (Mate)
import XMonad.Hooks.DynamicLog
import qualified DBus as D
import qualified DBus.Client as D
import qualified Codec.Binary.UTF8.String as UTF8

-- Layouts
import XMonad.Layout.IM
import XMonad.Layout.Grid
import XMonad.Layout.PerWorkspace (onWorkspaces)
import XMonad.Layout.Reflect (reflectHoriz)
import XMonad.Layout.NoFrillsDecoration
import XMonad.Layout.Tabbed
import Data.Ratio ((%))

-- Hooks
import XMonad.Hooks.ManageDocks  
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.SetWMName
import XMonad.Hooks.UrgencyHook

-- Window Mgmt
import XMonad.Actions.CopyWindow

-- main declaration
main :: IO ()
main = do
    dbus <- D.connectSession
    getWellKnownName dbus
    xmonad $ withUrgencyHookC BorderUrgencyHook { urgencyBorderColor = "#ff0000" } urgencyConfig { suppressWhen = Never }
	$ mateConfig {
	  focusFollowsMouse  = myFocusFollowsMouse
	, clickJustFocuses   = myClickJustFocuses
	, modMask = myModMask
	, workspaces = myWorkspaces
	, normalBorderColor  = myNormalBorderColor
        , focusedBorderColor = myFocusedBorderColor
	, layoutHook         = myLayout
        , manageHook         = myManageHook
        , startupHook        = myStartupHook
	, handleEventHook = myHandleEventHook
        , logHook = setWMName "LG3D" <+> -- Fixes problems with Java Swing applications
		 dynamicLogWithPP (prettyPrinter dbus)
	, keys			= \xconfig -> M.union (windowManagementKeys xconfig) (keys mateConfig xconfig)
         }

-- basic config
myModMask = mod4Mask
myWorkspaces = ["1:im","2:web","3:mail","4","5","6","7","8:vm","9:media"]
myNormalBorderColor  = "#dddddd"
myFocusedBorderColor = "#ff0000"
myFocusFollowsMouse = False
myClickJustFocuses = True
myStartupHook = do
	spawn "xmodmap ~/.Xmodmap"


-- layouts
imLayout theme = avoidStruts $ reflectHoriz $ ( withIM (1%7) pidgin $ withIM (1%6) skype $ Grid ) ||| tabbed shrinkText theme
    where pidgin = And (ClassName "Pidgin") (Role "buddy_list")
          skype = And (ClassName "Skype") $ And (Role "") (Not $ Title "Optionen")

generalLayout theme = avoidStruts $ (noFrillsDeco shrinkText theme $ Mirror tiled) ||| (noFrillsDeco shrinkText theme $ tiled) ||| tabbed shrinkText theme
  where
     -- default tiling algorithm partitions the screen into two panes
     tiled   = Tall nmaster delta ratio

     -- The default number of windows in the master pane
     nmaster = 1

     -- Default proportion of screen occupied by master pane
     ratio   = 5/9

     -- Percent of screen to increment by when resizing panes
     delta   = 3/100

myLayout = onWorkspaces ["1:im"] (imLayout theme) $ (generalLayout theme)
	where
	     -- Customise decoration theme
	     theme   = defaultTheme { activeColor = "black", fontName = "xft:Ubuntu-10", urgentColor = "red" }

-- window management: default workspaces
myManageHook = ( composeAll . concat $
    [ --className =? "MPlayer"        --> doFloat
    --, className =? "Gimp"           --> doFloat
    --, resource  =? "desktop_window" --> doIgnore
    --, resource  =? "kdesktop"       --> doIgnore
    --, className =? "Conky"          --> doIgnore ] 
    [className =? c      --> doShift "3:mail"  | c <- mailprogs ]
    , [className =? c      --> doShift "1:im"  | c <- improgs ]
    , [className =? c      --> doShift "8:vm"  | c <- vmprogs ]
    , [className =? c      --> doShift "9:media"  | c <- mediaprogs ]
    ]) <+> manageDocks
	where
		mailprogs = ["Thunderbird"]
		improgs   = ["Pidgin", "Slack", "Skype"]
		vmprogs   = ["VirtualBox"]
		mediaprogs = ["Spotify"]

-- window management: window copy
windowManagementKeys conf@(XConfig {modMask = modm}) = M.fromList $
    [((shiftMask .|. controlMask .|. modm, k), windows $ copy i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        ]


-- event handlers
myHandleEventHook = 
    fullscreenEventHook <+> docksEventHook


-- output for dbus / xmonad-log-applet
prettyPrinter :: D.Client -> PP
prettyPrinter dbus = defaultPP
    { ppOutput   = dbusOutput dbus
    , ppTitle    = const "" -- was: pangoSanitize; suppresses title output
    , ppCurrent  = pangoColor "green" . wrap "[" "]" . pangoSanitize
    , ppVisible  = pangoColor "yellow" . wrap "(" ")" . pangoSanitize
--    , ppHidden   = const ""
    , ppUrgent   = pangoColor "red"
    , ppLayout   = const ""
    , ppSep      = " "
    }

getWellKnownName :: D.Client -> IO ()
getWellKnownName dbus = do
  D.requestName dbus (D.busName_ "org.xmonad.Log")
                [D.nameAllowReplacement, D.nameReplaceExisting, D.nameDoNotQueue]
  return ()
  
dbusOutput :: D.Client -> String -> IO ()
dbusOutput dbus str = do
    let signal = (D.signal (D.objectPath_ "/org/xmonad/Log") (D.interfaceName_ "org.xmonad.Log") (D.memberName_ "Update")) {
            D.signalBody = [D.toVariant ("<b>" ++ (UTF8.decodeString str) ++ "</b>")]
        }
    D.emit dbus signal

pangoColor :: String -> String -> String
pangoColor fg = wrap left right
  where
    left  = "<span foreground=\"" ++ fg ++ "\">"
    right = "</span>"

pangoSanitize :: String -> String
pangoSanitize = foldr sanitize ""
  where
    sanitize '>'  xs = "&gt;" ++ xs
    sanitize '<'  xs = "&lt;" ++ xs
    sanitize '\"' xs = "&quot;" ++ xs
    sanitize '&'  xs = "&amp;" ++ xs
    sanitize x    xs = x:xs
