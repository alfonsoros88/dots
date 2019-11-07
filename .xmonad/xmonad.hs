import XMonad
import XMonad.Config.Desktop
import XMonad.Layout.Spacing
import XMonad.Util.EZConfig (additionalKeysP)

import Data.List (sortBy)
import Data.Function (on)
import Control.Monad (forM_, join)
import XMonad.Util.Run (safeSpawn)
import XMonad.Util.NamedWindows (getName)
import qualified XMonad.StackSet as W

main = do
    forM_ [".xmonad-workspace-log", ".xmonad-title-log"] $ \file -> do safeSpawn "mkfifo" ["/tmp/" ++ file]
    xmonad $ desktopConfig
        { terminal = "urxvt -e tmux"
        , layoutHook = spacingRaw True (Border 0 10 10 10) True (Border 10 10 10 10) True $ layoutHook desktopConfig
        , logHook = eventLogHook
        } 
        `additionalKeysP` 
        [ ("M-S-l", spawn "i3lock-fancy -p")
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
