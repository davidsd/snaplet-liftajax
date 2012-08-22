{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Snap.Snaplet.LiftAjax.Splice
    ( ajaxFormWithHandler
    , ajaxForm
    , ajaxFormWithSplices
    , ajaxButtonWithHandler
    , ajaxButton
    , ajaxButton_
    , ajaxJsonButton
    ) where

------------------------------------------------------------------------------
import           Control.Monad.Trans
import           Data.Aeson
import           Data.Function                  (on)
import           Data.List                      (unionBy)
import           Data.Monoid
import qualified Data.ByteString.Char8          as B
import qualified Data.ByteString.Lazy           as LB
import           Data.Text                      (Text)
import           Language.Javascript.JMacro
import           Safe
import           Snap.Snaplet
import           Snap.Snaplet.LiftAjax.Callback
import qualified Snap.Snaplet.LiftAjax.Js       as Js
import           Snap.Snaplet.LiftAjax.State
import           Text.Digestive
import           Text.Digestive.Snap
import           Text.Templating.Heist
import qualified Text.XmlHtml                   as X
------------------------------------------------------------------------------

type FormHandler b v a = Either (View v) a -> Handler b b JStat

-- Does not override existing attributes
addAttrs :: [(Text, Text)]  -- ^ Original attributes
         -> [(Text, Text)]  -- ^ Attributes to add
         -> [(Text, Text)]  -- ^ Resulting attributes
addAttrs = unionBy (on (==) fst)

lazyFromStrict :: B.ByteString -> LB.ByteString
lazyFromStrict = LB.fromChunks . (:[])

ajaxCall :: JExpr -> JStat
ajaxCall e = [jmacro| liftAjax.lift_ajaxHandler(`(e)`, null, null, 'javascript');
                      return false; |]

ajaxCallWithParams :: [(Text, JExpr)] -> JStat
ajaxCallWithParams ps = ajaxCall $ collect $ map pair ps
    where
      pair (a,b) = [jmacroE|`(a)`+"="+encodeURIComponent(`(b)`)|]
      collect = foldl1Def Js.null (\x xs -> [jmacroE|`(x)`+"&"+`(xs)`|])

liftAjax :: HasAjax b => AjaxHandler b a -> HeistT (Handler b b) a
liftAjax = lift . withTop ajaxLens

addAjaxCallback :: HasAjax b => Handler b b JStat -> HeistT (Handler b b) HandlerId
addAjaxCallback = liftAjax . addCallback . (>>= Js.write)

ajaxFormWithHandler :: HasAjax b =>
                       Handler b b JStat
                    -> Splice (Handler b b)
ajaxFormWithHandler h = do
  X.Element _ attrs cs <- getParamNode
  formId    <- liftAjax newRandomId
  handlerId <- addAjaxCallback h
  children  <- runNodeList cs
  let hidden = X.Element "input" [ ("type", "hidden")
                                 , ("name", hidAsText handlerId)
                                 , ("id",   hidAsText handlerId)
                                 ] []
      form = X.Element "form" (addAttrs [ ("action",   "javascript://")
                                        , ("onsubmit", Js.showAsText sendForm)
                                        , ("id",       formId)
                                        ] attrs) (children ++ [hidden])
      sendForm = ajaxCall [jmacroE|$(`("#"<>formId)`).serialize()|]
  return [form]

ajaxForm :: HasAjax b =>
            Text
         -> Form v (Handler b b) a
         -> FormHandler b v a
         -> Splice (Handler b b)
ajaxForm name form process =
    ajaxFormWithHandler $ do
      (view, result) <- runForm name form
      process (maybe (Left view) Right result)

ajaxFormWithSplices :: HasAjax b =>
                       (View v -> [(Text, Splice (Handler b b))])
                    -> Form v (Handler b b) a
                    -> FormHandler b v a
                    -> Splice (Handler b b)
ajaxFormWithSplices splices form process = do
  name <- liftAjax newRandomId
  view <- lift $ getForm name form
  localTS (bindSplices $ splices view) $ ajaxForm name form process

ajaxButtonWithHandler :: HasAjax b =>
                         [(Text, JExpr)]
                      -> Handler b b JStat
                      -> Splice (Handler b b)
ajaxButtonWithHandler jsParams h = do
  X.Element _ attrs cs <- getParamNode
  handlerId <- addAjaxCallback h
  children  <- runNodeList cs
  let button = X.Element "button" (addAttrs [ ("onclick", Js.showAsText call)
                                            ] attrs) children
      call = ajaxCallWithParams $ [(hidAsText handlerId, Js.null)] ++ jsParams
  return [button]

ajaxButtonWithParser :: HasAjax b =>
                        (B.ByteString -> Maybe a)
                     -> JExpr
                     -> (Maybe a -> Handler b b JStat)
                     -> Splice (Handler b b)
ajaxButtonWithParser parse jsExpr process = do
  exprName <- liftAjax newRandomId
  ajaxButtonWithHandler [(exprName, jsExpr)] $ do
                                    expr <- getTextRqParam exprName
                                    process $ expr >>= parse

ajaxButton :: (HasAjax b, Read a) =>
              JExpr
           -> (Maybe a -> Handler b b JStat)
           -> Splice (Handler b b)
ajaxButton = ajaxButtonWithParser $ readMay . B.unpack

ajaxJsonButton :: (HasAjax b, FromJSON a) =>
                  JExpr
               -> (Maybe a -> Handler b b JStat)
               -> Splice (Handler b b)
ajaxJsonButton jsonExpr = ajaxButtonWithParser (decode . lazyFromStrict)
                          [jmacroE|JSON.stringify(`(jsonExpr)`)|]

ajaxButton_ :: (HasAjax b) => Handler b b JStat -> Splice (Handler b b)
ajaxButton_ press = ajaxButtonWithHandler [] press

