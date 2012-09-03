module Snap.Snaplet.LiftAjax.State where

------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Concurrent.STM
import           Control.Monad.State
import           Data.ByteString             (ByteString)
import qualified Data.ByteString.Char8       as B
import           Data.Lens.Lazy
import           Data.Map                    (Map)
import           Data.Maybe
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Data.Time.Clock.POSIX
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Session.Common
------------------------------------------------------------------------------

newtype PageId     = PageId ByteString    deriving (Eq, Ord, Read, Show)
newtype CallbackId = CallbackId ByteString deriving (Eq, Ord, Read, Show)

type PageCallbacks b = Map CallbackId (Handler b b ())
type SiteCallbacks b = Map PageId     (PageCallbacks b)

type Heartbeats = Map PageId POSIXTime

data Ajax b = Ajax { ajaxHeartbeats   :: TVar Heartbeats
                   , ajaxCallbacks    :: TVar (SiteCallbacks b)
                   , ajaxPageId       :: Maybe PageId
                   , ajaxRNG          :: RNG
                   , ajaxPageLifetime :: POSIXTime
                   , ajaxGCDelay      :: Int
                   }

type AjaxHandler b = Handler b (Ajax b)

class HasAjax b where
    ajaxLens :: Lens b (Snaplet (Ajax b))

------------------------------------------------------------------------------

cidAsText :: CallbackId -> Text
cidAsText (CallbackId c) = T.pack $ B.unpack c

getRqParam :: ByteString -> Handler b v (Maybe ByteString)
getRqParam p = liftM (>>=listToMaybe) $ getsRequest $ rqParam p

getTextRqParam :: Text -> Handler b v (Maybe ByteString)
getTextRqParam = getRqParam . B.pack . T.unpack

getPageId :: AjaxHandler b PageId
getPageId = do
  mPageId <- gets ajaxPageId
  maybe genNewId return mPageId
      where genNewId = newPageId >>= setPageId >> getPageId

setPageId :: PageId -> AjaxHandler b ()
setPageId pageId = modify $ \a -> a { ajaxPageId = Just pageId }

newRandomId :: AjaxHandler b Text
newRandomId = gets ajaxRNG >>= liftIO . mkCSRFToken

newPageId :: AjaxHandler b PageId
newPageId = PageId . B.pack . T.unpack <$> newRandomId

newCallbackId :: AjaxHandler b CallbackId
newCallbackId = CallbackId . B.pack . T.unpack <$> newRandomId

getAjaxTVar :: (Ajax b -> TVar a) -> AjaxHandler b a
getAjaxTVar a = gets a >>= liftIO . readTVarIO

modifyAjaxTVar :: (Ajax b -> TVar a) -> (a -> a) -> AjaxHandler b ()
modifyAjaxTVar a f = gets a >>= liftIO . atomically . flip modifyTVar f

getCallbacks :: AjaxHandler b (SiteCallbacks b)
getCallbacks = getAjaxTVar ajaxCallbacks

modifyCallbacks :: (SiteCallbacks b -> SiteCallbacks b) -> AjaxHandler b ()
modifyCallbacks = modifyAjaxTVar ajaxCallbacks

getHeartbeats :: AjaxHandler b Heartbeats
getHeartbeats = getAjaxTVar ajaxHeartbeats

modifyHeartbeats :: (Heartbeats -> Heartbeats) -> AjaxHandler b ()
modifyHeartbeats = modifyAjaxTVar ajaxHeartbeats

