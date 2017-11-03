{-# LANGUAGE OverloadedStrings #-}

module UI.Index.Main where

import qualified Brick.Main as M
import Brick.Types (Padding(..), Widget)
import qualified Brick.Types as T
import Brick.AttrMap (AttrName)
import Brick.Widgets.Core
       (hLimit, padLeft, txt, vBox, vLimit, withAttr, (<+>))
import Prelude hiding (unwords)
import qualified Brick.Widgets.Edit        as E
import qualified Brick.Widgets.List        as L
import Control.Lens.Getter (view)
import Control.Lens.Lens ((&))
import Control.Lens.Setter (set)
import Graphics.Vty.Input.Events (Event)
import Data.Time.Clock (UTCTime(..))
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.Semigroup ((<>))
import Data.Text (Text, pack, unwords)
import UI.Draw.Main (editorDrawContent)
import UI.Keybindings (handleEvent)
import UI.Status.Main (statusbar)
import UI.Draw.Main (fillLine)
import Storage.Notmuch (mailIsNew)
import Types
import Config.Main
       (listNewMailAttr, listNewMailSelectedAttr, mailTagsAttr,
        listSelectedAttr, listAttr)

drawMain :: AppState -> [Widget Name]
drawMain s = [ui]
  where
    inputBox = renderEditor s
    ui = vBox [renderMailList s, statusbar s, vLimit 1 inputBox]

renderEditor :: AppState -> Widget Name
renderEditor s = let editorFocus = view asAppMode s `elem` [SearchThreads, ManageThreadTags, ManageMailTags]
                     render l = E.renderEditor editorDrawContent editorFocus  l
                 in case view asAppMode s of
                      ManageThreadTags -> render (view (asMailIndex . miThreadTagsEditor) s)
                      ManageMailTags -> render (view (asMailIndex . miMailTagsEditor) s)
                      _ -> render (view (asMailIndex . miSearchThreadsEditor) s)

renderMailList :: AppState -> Widget Name
renderMailList s =
  let listOfThreads = L.renderList (listDrawThread s) True $ view (asMailIndex . miListOfThreads) s
      listOfMails = L.renderList (listDrawMail s) True $ view (asMailIndex . miListOfMails) s
  in if (view asAppMode s `elem` [ManageThreadTags, BrowseThreads, SearchThreads]) then listOfThreads else listOfMails

listDrawMail :: AppState -> Bool -> NotmuchMail -> Widget Name
listDrawMail s sel a =
    let settings = view (asConfig . confNotmuch) s
        isNewMail = mailIsNew (view nmNewTag settings) (view mailTags a)
        widget = padLeft (Pad 1) $ hLimit 15 (txt $ view mailFrom a) <+>
                 padLeft (Pad 2) (txt (view mailSubject a)) <+>
                 padLeft Max (renderTagsWidget (view mailTags a) (view nmNewTag settings))
    in withAttr (getListAttr isNewMail sel) widget

listDrawThread :: AppState -> Bool -> NotmuchThread -> Widget Name
listDrawThread s sel a =
    let settings = view (asConfig . confNotmuch) s
        isNewMail = mailIsNew (view nmNewTag settings) (view thTags a)
        widget = padLeft (Pad 1) (txt $ formatDate (view thDate a)) <+>
                 padLeft (Pad 1) (txt $ pack $ "(" <> show (view thReplies a) <> ")") <+>
                 padLeft (Pad 1) (renderTagsWidget (view thTags a) (view nmNewTag settings)) <+>
                 padLeft (Pad 1) (hLimit 15 (txt $ unwords $ view thAuthors a)) <+>
                 padLeft (Pad 1) (txt (view thSubject a)) <+> fillLine
    in withAttr (getListAttr isNewMail sel) widget

getListAttr :: Bool  -- ^ new?
            -> Bool  -- ^ selected?
            -> AttrName
getListAttr True True = listNewMailSelectedAttr  -- new and selected
getListAttr True False = listNewMailAttr  -- new and not selected
getListAttr False True = listSelectedAttr  -- not new but selected
getListAttr False False = listAttr  -- not selected and not new

formatDate :: UTCTime -> Text
formatDate t = pack $ formatTime defaultTimeLocale "%d/%b" (utctDay t)

renderTagsWidget :: [Text] -> Text -> Widget Name
renderTagsWidget tgs ignored =
    let ts = filter (/= ignored) tgs
    in withAttr mailTagsAttr $ vLimit 1 $ txt $ unwords ts

-- | We currently have two modes on the main view we need to distinguish
-- keystrokes for. One is to obviously browse the mails which are shown as a
-- list, the other is to allow the user to easily change the list.
mainEvent :: AppState -> Event -> T.EventM Name (T.Next AppState)
mainEvent s =
    case view asAppMode s of
        BrowseMail ->
            handleEvent
                (view (asConfig . confIndexView . ivBrowseMailsKeybindings) s)
                listEventDefault
                s
        ManageThreadTags ->
            handleEvent
                (view (asConfig . confIndexView . ivManageThreadTagsKeybindings) s)
                searchInputEventDefault
                s
        ManageMailTags ->
            handleEvent
                (view (asConfig . confIndexView . ivManageMailTagsKeybindings) s)
                (\_ e -> M.continue =<< T.handleEventLensed s (asMailIndex . miMailTagsEditor) E.handleEditorEvent e)
                s
        BrowseThreads ->
            handleEvent
            (view (asConfig . confIndexView . ivBrowseThreadsKeybindings) s)
            (\_ e -> M.continue =<< T.handleEventLensed s (asMailIndex . miListOfThreads) L.handleListEvent e)
                s
        _ ->
            handleEvent
                (view (asConfig . confIndexView . ivSearchThreadsKeybindings) s)
                searchInputEventDefault
                s

-- | Handle key strokes on the list of mails.
-- Most keystrokes are delegated to the list of mails, while one particular
-- get's us out of the application and one is to signal that we want the focus
-- on the input field to change the notmuch search.
-- Note: moving around in the list clears any errors set during other events.
-- Maybe that will remove anything important for the user to see. Do we need
-- something like an error log?
listEventDefault :: AppState -> Event -> T.EventM Name (T.Next AppState)
listEventDefault s e =
    L.handleListEvent e (view (asMailIndex . miListOfMails) s) >>=
    \mi' ->
         M.continue $
         set (asMailIndex . miListOfMails) mi' s & set asAppMode BrowseMail &
         set asError Nothing


-- | Search search input is mostly straight forward, since every keystroke is
-- delegated to the widget, except applying the entered input or signaling to
-- move the focus back to the list of mails.
searchInputEventDefault :: AppState -> Event -> T.EventM Name (T.Next AppState)
searchInputEventDefault s ev =
    E.handleEditorEvent ev (view (asMailIndex . miSearchThreadsEditor) s) >>=
    \ed ->
         M.continue $
         set (asMailIndex . miSearchThreadsEditor) ed s
