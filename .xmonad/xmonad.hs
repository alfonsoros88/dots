import XMonad
import XMonad.Config.Desktop
import XMonad.Layout.Spacing
import XMonad.Util.EZConfig (additionalKeysP)

import Data.List (sortBy)
import Data.Function (on)
import Control.Monad (forM_, join)
import XMonad.Util.Run (safeSpawn)
import XMonad.Util.NamedWindows (getName)
import XMonad.Hooks.UrgencyHook
import qualified XMonad.StackSet as W

data LibNotifyUrgencyHook = LibNotifyUrgencyHook deriving (Read, Show)

instance UrgencyHook LibNotifyUrgencyHook where
    urgencyHook LibNotifyUrgencyHook w = do
        name <- getName w
        Just idx <- fmap (W.findTag w) $ gets windowset
        safeSpawn "notify-send" [show name, "workspace " ++ idx]

main = do
    forM_ [".xmonad-workspace-log", ".xmonad-title-log"] $ \file -> do safeSpawn "mkfifo" ["/tmp/" ++ file]
    xmonad $ withUrgencyHook LibNotifyUrgencyHook $ desktopConfig
        { terminal = "kitty tmux -Lkitty"
        , layoutHook = spacingRaw True (Border 0 10 10 10) True (Border 10 10 10 10) True $ layoutHook desktopConfig
        , logHook = eventLogHook
        , focusedBorderColor = "#98971a"
        } 
        `additionalKeysP` 
        [ ("M-S-l", spawn "i3lock-fancy -p")
        , ("M-p", spawn "rofi -show run")
        ]

eventLogHook = do
    winset <- gets windowset
    title <- maybe (return "") (fmap show . getName) . W.peek $ winset
    let currWs = W.currentTag winset
    let wss = map W.tag $ W.workspaces winset
    let wsStr = join $ map (fmt currWs) $ sort' wss

    io $ appendFile "/tmp/.xmonad-title-log" (title ++ "\n")
    io $ appendFile "/tmp/.xmonad-workspace-log" (wsStr ++ "\n")

    where fmt currWs ws
            | currWs == ws = "[" ++ ws ++ "]"
            | otherwise = " " ++ ws ++ " "
          sort' = sortBy (compare `on` (!! 0))
