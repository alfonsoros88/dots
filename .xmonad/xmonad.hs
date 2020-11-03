import XMonad
import XMonad.Config.Desktop
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Layout.Spacing
import XMonad.Actions.CycleWS (toggleWS, nextWS, prevWS)

-- space between windows
myLayoutHook = spacingRaw True (Border 0 10 10 10) True (Border 10 10 10 10) True

main = xmonad $ desktopConfig
    { terminal   = "alacritty"
    , modMask    = mod4Mask
    , layoutHook = myLayoutHook $ layoutHook desktopConfig
    } `additionalKeys`
    [ ((mod4Mask, xK_p), spawn "rofi -show run")
    , ((mod4Mask .|. shiftMask, xK_l), spawn "i3lock-fancy --pixelate")
    , ((mod4Mask, xK_Tab), toggleWS)
    , ((mod4Mask, xK_h), prevWS)
    , ((mod4Mask, xK_l), nextWS)]
