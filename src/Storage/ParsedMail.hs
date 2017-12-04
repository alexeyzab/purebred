{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- | module to integrate with a mail parser. This is needed to actually view the
-- entire mail and it's attachments.
module Storage.ParsedMail where

import Codec.MIME.Parse (parseMIMEMessage)
import Codec.MIME.Type
       (MIMEContent(..), MIMEParam(..), MIMEValue(..), mime_val_headers,
        mimeParams, mime_val_headers, showMIMEParams, showMultipart,
        showType)
import Control.Exception (try)
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Text as T
import Data.Text.IO (readFile)
import Data.Vector (Vector, singleton, empty, cons, snoc)
import Prelude hiding (readFile)
import Storage.Notmuch (mailFilepath)

import Error
import Types

parseMail
  :: (MonadError Error m, MonadIO m)
  => NotmuchMail -> FilePath -> m ParsedMail
parseMail m dbpath = do
  filePath <- mailFilepath m dbpath
  liftIO (try (readFile filePath))
    >>= either
      (throwError . FileReadError filePath)
      (pure . MIMEMail . parseMIMEMessage)

getFrom :: ParsedMail -> T.Text
getFrom (MIMEMail v) = findHeader v "from"
getFrom _ = throwNotImplemented

getSubject :: ParsedMail -> T.Text
getSubject (MIMEMail v) = findHeader v "subject"
getSubject _ = throwNotImplemented

getTo :: ParsedMail -> T.Text
getTo (MIMEMail v) = findHeader v "to"
getTo _ = throwNotImplemented

getAttachments :: ParsedMail -> Vector Attachment
getAttachments (MIMEMail v) = getAttachments' $ mime_val_content v
getAttachments _ = throwNotImplemented

getAttachments' :: MIMEContent -> Vector Attachment
getAttachments' (Single c) = singleton $ Attachment "<no description>" c
getAttachments' (Multi vs) = foldl (\a x -> a `snoc` Attachment (filename $ mimeParams $ mime_val_type x) (showType $ mime_val_type x)) empty vs
  where
    filename :: [MIMEParam] -> T.Text
    filename xs = T.concat $ paramValue <$> filter (\x -> (paramName x) == "name") xs

throwNotImplemented :: a
throwNotImplemented = error "Not implemented. ParsedMail.hs needs a proper mail parser"

findHeader :: MIMEValue -> T.Text -> T.Text
findHeader m name = T.strip . paramValue . head $ filter (\x -> paramName x == name) $ mime_val_headers m
