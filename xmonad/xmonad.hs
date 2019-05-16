{-# LANGUAGE FlexibleContexts #-}

import XMonad
import XMonad.Config.Mate
import qualified Data.Map as M

-- For xmonad-log-applet (Mate)
import XMonad.Hooks.DynamicLog
import qualified DBus as D
import qualified DBus.Client as D
import qualified Codec.Binary.UTF8.String as UTF8
import XMonad.Util.Run

-- Layouts
import XMonad.Layout.IM
import XMonad.Layout.Grid
import XMonad.Layout.StackTile
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
import XMonad.ManageHook
import qualified XMonad.StackSet as W

-- Window Mgmt
import XMonad.Actions.CopyWindow
import XMonad.Actions.CycleWS
import XMonad.Actions.GridSelect

-- main declaration
main :: IO ()
main = do
    dbus <- D.connectSession
    getWellKnownName dbus
    xmonad $ withUrgencyHookC BorderUrgencyHook { urgencyBorderColor = red } urgencyConfig { suppressWhen = Never }
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
myNormalBorderColor  = base02
myFocusedBorderColor = base2
myFocusFollowsMouse = False
myClickJustFocuses = True
myStartupHook = do
	spawn "xmodmap ~/.Xmodmap"


-- layouts
imLayout theme = avoidStruts $ reflectHoriz $ ( imColumns $ StackTile 1 (3/100) (1/2) ) ||| ( imColumns $ tabbed shrinkText theme )
    where pidgin = And (ClassName "Pidgin") (Role "buddy_list")
          skype = And (ClassName "Skype") $ And (Role "") (Not $ Title "Optionen")
          imColumns n = withIM (1%7) pidgin $ withIM (1%6) skype $ n

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

mailLayout theme = avoidStruts $ (Mirror $ splitLayout) ||| splitLayout ||| tabbed shrinkText theme
        where thunderbirdMain = And (ClassName "Thunderbird") (Role "3pane")
              splitLayout = withIM (1%2) thunderbirdMain $ (Mirror $ tabbed shrinkText theme)


myLayout = onWorkspaces ["1:im"] (imLayout theme) $
             onWorkspaces ["3:mail"] (mailLayout theme) $
             (generalLayout theme)
	where
	     -- Customise decoration theme
	     theme   = defaultTheme { activeColor = base3, inactiveColor = base03, activeBorderColor = base2, inactiveBorderColor = base02, inactiveTextColor = base0, activeTextColor = base00, fontName = "xft:Ubuntu:style=Medium:size=10:antialias=true", urgentColor = red }


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
    , [title =? "VirtualBox" --> doIgnore ]
    , [role =? "GtkFileChooserDialog" --> doSink]
    ]) <+> manageDocks
	where
                role = stringProperty "WM_WINDOW_ROLE"
		mailprogs = ["Thunderbird", "Feedreader"]
		improgs   = ["Pidgin", "Slack", "Skype"]
		vmprogs   = ["VirtualBox"]
		mediaprogs = ["Spotify"]

-- window management: additional managehook
doSink :: ManageHook
doSink = ask >>= \w -> liftX (reveal w) >> doF (W.sink w)

-- window management: additional keys
windowManagementKeys conf@(XConfig {modMask = modm}) = M.fromList $
    -- select password (pass)
    ((modm .|. shiftMask, xK_p), spawn "passmenu")
    :
    -- toggle workspace
    ((modm, xK_z), toggleWS)
    :
    -- grid select
    ((modm, xK_o), goToSelected defaultGSConfig)
    :
    -- window copy
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
    , ppTitle    = const ""
    , ppCurrent  = pangoColor "green" . wrap "[" "]" . pangoSanitize
    , ppVisible  = pangoColor "yellow" . wrap "(" ")" . pangoSanitize
--    , ppHidden   = const ""
    , ppUrgent   = pangoColor "red"
    , ppLayout   = const ""
    , ppSep      = " "
    }

spotifyTitle :: IO String
spotifyTitle = runProcessWithInput "/usr/bin/python3" [".xmonad/DBus-spotify.py", "print_info", "spotify"] ""
--spotifyTitle = runProcessWithInput "date" [] ""

getWellKnownName :: D.Client -> IO ()
getWellKnownName dbus = do
  D.requestName dbus (D.busName_ "org.xmonad.Log")
                [D.nameAllowReplacement, D.nameReplaceExisting, D.nameDoNotQueue]
  return ()
  
dbusOutput :: D.Client -> String -> IO ()
dbusOutput dbus str = do
    spotify <- spotifyTitle
    let
        title | length spotify == 0 = ""
              | otherwise = init spotify
        signal = (D.signal (D.objectPath_ "/org/xmonad/Log") (D.interfaceName_ "org.xmonad.Log") (D.memberName_ "Update")) {
            D.signalBody = [D.toVariant ("<b>" ++ (UTF8.decodeString str) ++ "</b>  " ++ title)]
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

-- solarized colors
base03 = "#002b36"
base02 = "#073642"
base01 = "#586e75"
base00 = "#657b83"
base0  = "#839496"
base1  = "#93a1a1"
base2  = "#eee8d5"
base3  = "#fdf6e3"
yellow = "#b58900" 
orange = "#cb4b16" 
red    = "#dc322f" 
magenta= "#d33682"
violet = "#6c71c4"
blue   = "#268bd2"
cyan   = "#2aa198"
green  = "#859900"
