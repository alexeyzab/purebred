{-# LANGUAGE DataKinds #-}

module UI.Index.Keybindings where

import qualified Brick.Types as Brick
import qualified Graphics.Vty as V
import UI.Actions
import Types

-- | Default Keybindings
browseMailKeybindings :: [Keybinding 'BrowseMail (Brick.Next AppState)]
browseMailKeybindings =
    [ Keybinding (V.EvKey V.KEsc []) (noop `chain'` (focus :: Action 'BrowseThreads AppState) `chain'` (reloadList :: Action 'BrowseThreads AppState) `chain` continue)
    , Keybinding (V.EvKey V.KEnter []) (displayMail `chain` continue)
    , Keybinding (V.EvKey V.KDown []) (mailIndexDown `chain` continue)
    , Keybinding (V.EvKey V.KUp []) (mailIndexUp `chain` continue)
    , Keybinding (V.EvKey (V.KChar 'r') []) (replyMail `chain` continue)
    , Keybinding (V.EvKey (V.KChar 't') []) (setUnread `chain` continue)
    , Keybinding (V.EvKey (V.KChar '?') []) (noop `chain'` (focus :: Action 'Help AppState) `chain` continue)
    , Keybinding (V.EvKey (V.KChar '`') []) (noop `chain'` (focus :: Action 'ManageMailTags AppState) `chain` continue)
    ]

browseThreadsKeybindings :: [Keybinding 'BrowseThreads (Brick.Next AppState)]
browseThreadsKeybindings =
    [ Keybinding (V.EvKey V.KEsc []) quit
    , Keybinding (V.EvKey V.KEnter []) (displayThreadMails `chain'` (focus :: Action 'BrowseMail AppState) `chain` continue)
    , Keybinding (V.EvKey (V.KChar ':') []) (noop `chain'` (focus :: Action 'SearchThreads AppState) `chain` continue)
    , Keybinding (V.EvKey (V.KChar '?') []) (viewHelp `chain` continue)
    , Keybinding (V.EvKey (V.KChar 'm') []) (composeMail `chain` continue)
    , Keybinding (V.EvKey (V.KChar '\t') []) (switchComposeEditor `chain` continue)
    ]

searchThreadsKeybindings :: [Keybinding 'SearchThreads (Brick.Next AppState)]
searchThreadsKeybindings =
    [ Keybinding (V.EvKey V.KEsc []) (backToIndex `chain` continue)
    , Keybinding (V.EvKey V.KEnter []) (done `chain'` (focus :: Action 'BrowseThreads AppState) `chain` continue)
    ]

manageMailTagsKeybindings :: [Keybinding 'ManageMailTags (Brick.Next AppState)]
manageMailTagsKeybindings =
    [ Keybinding (V.EvKey V.KEsc []) (abort `chain'` (focus :: Action 'BrowseMail AppState) `chain` continue)
    , Keybinding (V.EvKey V.KEnter []) (done `chain'` (focus :: Action 'BrowseMail AppState) `chain` continue)
    ]

manageThreadTagsKeybindings :: [Keybinding 'ManageThreadTags (Brick.Next AppState)]
manageThreadTagsKeybindings =
    [ Keybinding (V.EvKey V.KEsc []) (abort `chain'` (focus :: Action 'BrowseThreads AppState) `chain` continue)
    , Keybinding (V.EvKey V.KEnter []) (done `chain'` (focus :: Action 'BrowseThreads AppState) `chain` continue)
    ]
