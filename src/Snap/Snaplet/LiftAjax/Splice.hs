{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Snap.Snaplet.LiftAjax.Splice
    ( ajaxForm ) where

------------------------------------------------------------------------------
import           Control.Monad.Trans
import           Data.Function                  (on)
import           Data.Lens.Lazy
import           Data.List                      (unionBy)
import           Data.Monoid
import           Data.Text                      (Text)
import           Language.Javascript.JMacro
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.LiftAjax.Callback
import qualified Snap.Snaplet.LiftAjax.Js       as Js
import           Snap.Snaplet.LiftAjax.State
import           Text.Digestive
import           Text.Digestive.Snap
import           Text.Templating.Heist
import qualified Text.XmlHtml                   as X
------------------------------------------------------------------------------

-- Does not override existing attributes
addAttrs :: [(Text, Text)]  -- ^ Original attributes
         -> [(Text, Text)]  -- ^ Attributes to add
         -> [(Text, Text)]  -- ^ Resulting attributes
addAttrs = unionBy (on (==) fst)

handleForm :: MonadSnap m =>
              Text
           -> Form v m a
           -> (Either (View v) a -> m JStat)
           -> m ()
handleForm name form process = do
  (view, result) <- runForm name form
  process (maybe (Left view) Right result) >>= Js.write

formWithHandler :: Lens b (Snaplet (Ajax b))
                -> Handler b b ()
                -> Splice (Handler b b)
formWithHandler ajax h = do
  X.Element _ attrs cs <- getParamNode
  formId    <- lift $ withTop ajax newRandomId
  handlerId <- lift $ withTop ajax $ addCallback h
  children  <- runNodeList cs
  return $ formNodes formId handlerId attrs children

formNodes :: Text -> HandlerId -> [(Text, Text)] -> [X.Node] -> [X.Node]
formNodes formId hid attrs children = [form]
    where
      hidden = X.Element "input" [ ("type", "hidden")
                                 , ("name", hidAsText hid)
                                 , ("id",   hidAsText hid)
                                 ] []
      form = X.Element "form" (addAttrs [ ("action",   "javascript://")
                                        , ("onsubmit", Js.showAsText sendForm)
                                        , ("id",       formId)
                                        ] attrs) (children ++ [hidden])
      sendForm = [jmacro| liftAjax.lift_ajaxHandler($(`("#"<>formId)`).serialize(),
                                                     null,
                                                     null,
                                                    'javascript');
                  return false; |]

ajaxForm :: Lens b (Snaplet (Ajax b))
         -> Text
         -> Form v (Handler b b) a
         -> (Either (View v) a -> Handler b b JStat)
         -> Splice (Handler b b)
ajaxForm ajax n f p = formWithHandler ajax $ handleForm n f p

