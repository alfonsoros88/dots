import XMonad
import XMonad.Config.Desktop
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Layout.Spacing
import XMonad.Actions.CycleWS (toggleWS, nextWS, prevWS)

-- space between windows
myLayoutHook = spacingRaw True (Border 0 10 10 10) True (Border 10 10 10 10) True

main = xmonad $ desktopConfig
    { terminal   = "kitty"
    , modMask    = mod4Mask
    , layoutHook = myLayoutHook $ layoutHook desktopConfig
    } `additionalKeys`
    [ ((mod4Mask, xK_p), spawn "rofi -show run")
    , ((mod4Mask .|. shiftMask, xK_l), spawn "i3lock-fancy --pixelate")
    , ((mod4Mask, xK_Tab), toggleWS)
    , ((mod4Mask, xK_Left), prevWS)
    , ((mod4Mask, xK_Right), nextWS)
    , ((0       , 0x1008FF17), spawn "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Next")
    , ((0       , 0x1008FF14), spawn "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.PlayPause")
    , ((0       , 0x1008FF16), spawn "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Previous")
    , ((0       , 0x1008FF11), spawn "amixer -D pulse set Master 2%-")
    , ((0       , 0x1008FF13), spawn "amixer -D pulse set Master 2%+")
    , ((0       , 0x1008FF12), spawn "amixer -D pulse set Master toggle")]
