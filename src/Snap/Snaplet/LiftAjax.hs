{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Snap.Snaplet.LiftAjax
    ( ajaxInit
    , defaultAjaxState
    , Ajax
    , HasAjax
    , ajaxLens
    ) where

------------------------------------------------------------------------------
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad.State
import           Data.ByteString                (ByteString)
import qualified Data.ByteString.Char8          as B
import qualified Data.Map                       as Map
import           Data.Monoid
import           Data.Text                      (Text)
import           Language.Javascript.JMacro
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Heist
import           Snap.Snaplet.LiftAjax.Callback
import qualified Snap.Snaplet.LiftAjax.Js       as Js
import           Snap.Snaplet.LiftAjax.State
import           Snap.Snaplet.Session.Common
import qualified Text.XmlHtml                   as X
------------------------------------------------------------------------------

defaultAjaxState :: IO (Ajax b)
defaultAjaxState = do
  heartbeats <- newTVarIO Map.empty
  callbacks  <- newTVarIO Map.empty
  rng        <- mkRNG
  return Ajax { ajaxHeartbeats   = heartbeats
              , ajaxCallbacks    = callbacks
              , ajaxPageId       = PageId ""
              , ajaxRNG          = rng
              , ajaxPageLifetime = 75*4           -- 5 minutes
              , ajaxGCDelay      = 75*4*1000*1000 -- 5 minutes
              }

ajaxInit :: HasHeist b => IO (Ajax b) -> SnapletInit b (Ajax b)
ajaxInit ajaxState = makeSnaplet "ajax" "" Nothing $ do
                       addSplices splices
                       addRoutes routes
                       wrapSite (setNewPageId >>)
                       ajax <- liftIO ajaxState
                       gcThread <- liftIO $ forkIO $ collector ajax
                       onUnload $ killThread gcThread
                       return ajax

routes :: HasHeist b => [(ByteString, AjaxHandler b ())]
routes = [ ("/request/:pageId/", handleRequest)
         , ("/gc",               ifLocal handleGC)
         , ("/state",            ifLocal handleState)
         ]
    where
      ifLocal m = do
        rip <- liftM rqRemoteAddr getRequest
        if rip `elem` [ "127.0.0.1" , "localhost" , "::1" ]
          then m
          else pass

splices :: [(Text, SnapletSplice b (Ajax b))]
splices = [ ("ajaxFooter", footerSplice) ]

footerSplice :: SnapletSplice b (Ajax b)
footerSplice = do
  PageId pid  <- liftHandler $ gets ajaxPageId
  ajaxUrl <- liftHandler getSnapletRootURL
  let initGC = [jmacro| jQuery(document).ready(function() {liftAjax.lift_successRegisterGC();});
                        var !lift_page = `(B.unpack pid)`;
                        var !lift_ajaxUrl = `(B.unpack ajaxUrl)`; |]
  return [ X.Element "script" [ ("type", "text/javascript") ]
           [X.TextNode $ "// <![CDATA[\n" <> Js.showAsText initGC <> "\n//]]>"]
         ]
