{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Snap.Snaplet.LiftAjax.Callback
    ( addCallback
    , handleRequest
    , handleGC
    , collector
    ) where

------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Category
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad.State
import qualified Data.Map                       as Map
import           Data.Maybe
import           Data.Time.Clock.POSIX
import           Prelude                        hiding (id, (.))
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.LiftAjax.State
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- Callbacks
------------------------------------------------------------------------------

addCallback :: Handler b b () -> AjaxHandler b CallbackId
addCallback c = do
  touchPage
  callbackId <- newCallbackId
  pageId <- getPageId
  modifyCallbacks $ insertCallback pageId callbackId
  return callbackId
      where
        insertCallback pid cid = Map.insertWith Map.union pid $ Map.singleton cid c

runCallback :: AjaxHandler b ()
runCallback = do
  maybeCallbacks <- Map.lookup <$> getPageId <*> getCallbacks
  params <- getsRequest rqParams
  withTop' id $ fromMaybe pass $ do
    callbacks <- maybeCallbacks
    listToMaybe $ mapMaybe (flip Map.lookup callbacks . CallbackId) (Map.keys params)

handleRequest :: AjaxHandler b ()
handleRequest = do
  maybePageId <- getRqParam "pageId"
  maybe pass (setPageId . PageId) maybePageId
  touchPage
  maybeGC <- getRqParam "__lift__GC"
  when (isNothing maybeGC) runCallback

touchPage :: AjaxHandler b ()
touchPage = do
  pageId <- getPageId
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
collect ajax = stalePages ajax >>= mapM_ (deletePage ajax)

collector :: Ajax b -> IO ()
collector ajax = go
    where go = collect ajax >> threadDelay (ajaxGCDelay ajax) >> go

handleGC :: AjaxHandler b ()
handleGC = get >>= liftIO . collect
