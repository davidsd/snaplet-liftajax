{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards   #-}

module Snap.Snaplet.LiftAjax.Callback
    ( addCallback
    , handleRequest
    , handleState
    , handleGC
    , collector
    ) where

------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Category
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad.State
import           Data.ByteString             (ByteString)
import qualified Data.ByteString.Char8       as B
import qualified Data.Map                    as Map
import           Data.Maybe
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Data.Time.Clock.POSIX
import           Data.Time.Clock             (NominalDiffTime)
import           Prelude                     hiding (id, (.))
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Heist
import           Snap.Snaplet.LiftAjax.State
import qualified Text.PrettyPrint            as PP
import           Text.PrettyPrint.HughesPJClass
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- Callbacks
------------------------------------------------------------------------------

addCallback :: Handler b b () -> AjaxHandler b HandlerId
addCallback h = do
  touchPage
  handlerId <- newHandlerId
  pageId <- gets ajaxPageId
  modifyCallbacks $ insertCallback pageId handlerId
  return handlerId
      where
        insertCallback pid hid = Map.insertWith Map.union pid $ Map.singleton hid h

runCallback :: AjaxHandler b ()
runCallback = do
  maybeCallbacks <- Map.lookup <$> gets ajaxPageId <*> getCallbacks
  params <- getsRequest rqParams
  withTop' id $ fromMaybe pass $ do
    callbacks <- maybeCallbacks
    listToMaybe $ catMaybes $ map (flip Map.lookup callbacks . HandlerId) (Map.keys params)

handleRequest :: AjaxHandler b ()
handleRequest = do
  maybePageId <- getRqParam "pageId"
  maybe pass (setPageId . PageId) maybePageId
  touchPage
  maybeGC <- getRqParam "__lift__GC"
  when (isNothing maybeGC) runCallback

touchPage :: AjaxHandler b ()
touchPage = do
  pageId <- gets ajaxPageId
  currentTime <- liftIO getPOSIXTime
  modifyHeartbeats $ Map.insert pageId currentTime

------------------------------------------------------------------------------
-- Garbage Collection
------------------------------------------------------------------------------

deletePage :: Ajax b -> PageId -> IO ()
deletePage (Ajax {..}) pageId =
  atomically $ do
    modifyTVar ajaxHeartbeats (Map.delete pageId)
    modifyTVar ajaxCallbacks  (Map.delete pageId)

stalePages :: Ajax b -> IO [PageId]
stalePages (Ajax {..}) = do
  currentTime <- getPOSIXTime
  heartbeats <- readTVarIO ajaxHeartbeats
  return $ map fst
             $ filter ((< currentTime - ajaxPageLifetime) . snd)
             $ Map.toList heartbeats

collect :: Ajax b -> IO ()
collect ajax = stalePages ajax >>= notify >>= mapM_ (deletePage ajax)
    where
      notify x = putStr "Collecting pages: " >> print x >> return x

collector :: Ajax b -> IO ()
collector ajax = go
    where go = collect ajax >> threadDelay (ajaxGCDelay ajax) >> go

handleGC :: AjaxHandler b ()
handleGC = get >>= liftIO . collect

------------------------------------------------------------------------------
-- Debugging
------------------------------------------------------------------------------

handleState :: HasHeist b => AjaxHandler b ()
handleState = showAjax >>= writeText

instance (Pretty key, Pretty val) => Pretty (Map.Map key val) where
    pPrint = PP.braces . PP.vcat . map kvPair . Map.toList
        where kvPair (k,v) = PP.hsep [pPrint k, PP.text "->", pPrint v]
instance Pretty ByteString       where pPrint = pPrint . B.unpack
instance Pretty NominalDiffTime  where pPrint = PP.text . show
instance Pretty (Handler b b ()) where pPrint = const $ PP.text "<handler>"
instance Pretty PageId           where pPrint = PP.text . show
instance Pretty HandlerId        where pPrint = PP.text . show

showAjax :: AjaxHandler b Text
showAjax = do
  Ajax {..} <- get
  hs <- liftIO $ readTVarIO ajaxHeartbeats
  cs <- liftIO $ readTVarIO ajaxCallbacks
  return $ T.pack $ PP.render $
           PP.vcat [ PP.text "Callbacks:"
                   , PP.nest 4 $ pPrint cs
                   , PP.text "Heartbeats:"
                   , PP.nest 4 $ pPrint hs
                   ]
