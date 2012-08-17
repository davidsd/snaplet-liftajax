module Snap.Snaplet.LiftAjax.State where

------------------------------------------------------------------------------
import           Control.Concurrent.STM
import           Control.Monad.State
import           Data.ByteString             (ByteString)
import qualified Data.ByteString.Char8       as B
import           Data.Map                    (Map)
import           Data.Maybe
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Data.Time.Clock.POSIX
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Session.Common
------------------------------------------------------------------------------

newtype PageId    = PageId ByteString    deriving (Eq, Ord, Read, Show)
newtype HandlerId = HandlerId ByteString deriving (Eq, Ord, Read, Show)

type PageCallbacks b = Map HandlerId (Handler b b ())
type SiteCallbacks b = Map PageId    (PageCallbacks b)

type Heartbeats = Map PageId POSIXTime

data Ajax b = Ajax { ajaxHeartbeats   :: TVar Heartbeats
                   , ajaxCallbacks    :: TVar (SiteCallbacks b)
                   , ajaxPageId       :: PageId
                   , ajaxRNG          :: RNG
                   , ajaxPageLifetime :: POSIXTime
                   , ajaxGCDelay      :: Int
                   }

type AjaxHandler b = Handler b (Ajax b)

------------------------------------------------------------------------------

hidAsText :: HandlerId -> Text
hidAsText (HandlerId h) = T.pack $ B.unpack h

getRqParam :: ByteString -> AjaxHandler b (Maybe ByteString)
getRqParam p = liftM (>>=listToMaybe) $ getsRequest $ rqParam p

setPageId :: PageId -> AjaxHandler b ()
setPageId pageId = modify $ \a -> a { ajaxPageId = pageId }

setNewPageId :: AjaxHandler b ()
setNewPageId = newRandomId >>= setPageId . PageId . B.pack . T.unpack

newHandlerId :: AjaxHandler b HandlerId
newHandlerId = fmap (HandlerId . B.pack . T.unpack) newRandomId

newRandomId :: AjaxHandler b Text
newRandomId = gets ajaxRNG >>= liftIO . mkCSRFToken

getAjaxTVar :: (Ajax b -> TVar a) -> AjaxHandler b a
getAjaxTVar a = gets a >>= liftIO . readTVarIO

modifyAjaxTVar :: (Ajax b -> TVar a) -> (a -> a) -> AjaxHandler b ()
modifyAjaxTVar a f = gets a >>= liftIO . atomically . (flip modifyTVar f)

getCallbacks :: AjaxHandler b (SiteCallbacks b)
getCallbacks = getAjaxTVar ajaxCallbacks

modifyCallbacks :: (SiteCallbacks b -> SiteCallbacks b) -> AjaxHandler b ()
modifyCallbacks = modifyAjaxTVar ajaxCallbacks

getHeartbeats :: AjaxHandler b Heartbeats
getHeartbeats = getAjaxTVar ajaxHeartbeats

modifyHeartbeats :: (Heartbeats -> Heartbeats) -> AjaxHandler b ()
modifyHeartbeats = modifyAjaxTVar ajaxHeartbeats

