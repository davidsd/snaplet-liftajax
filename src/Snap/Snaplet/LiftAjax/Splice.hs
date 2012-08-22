{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Snap.Snaplet.LiftAjax.Splice
    ( ajaxFormWithHandler
    , ajaxForm
    , ajaxFormWithSplices
    , ajaxElemWithHandler
    , ajaxElemWithParser
    , ajaxButton
    , ajaxButton_
    , ajaxJsonButton
    , ajaxAnchor
    , ajaxAnchor_
    , ajaxJsonAnchor
    ) where

------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Monad.Trans
import           Data.Aeson
import qualified Data.ByteString.Char8          as B
import qualified Data.ByteString.Lazy           as LB
import           Data.Function                  (on)
import           Data.List                      (unionBy)
import           Data.Monoid
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
type ButtonHandler b a = Maybe a -> Handler b b JStat

type Attrs    = [(Text, Text)]
type Children = [X.Node]

-- Does not override existing attributes
addAttrs :: Attrs  -- ^ Original attributes
         -> Attrs  -- ^ Attributes to add
         -> Attrs  -- ^ Resulting attributes
addAttrs = unionBy (on (==) fst)

lazyFromStrict :: B.ByteString -> LB.ByteString
lazyFromStrict = LB.fromChunks . pure

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

------------------------------------------------------------------------------
-- Forms
------------------------------------------------------------------------------

ajaxFormWithHandler :: HasAjax b => Handler b b JStat -> Splice (Handler b b)
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

------------------------------------------------------------------------------
-- Elements
------------------------------------------------------------------------------

ajaxElemWithHandler :: HasAjax b =>
                       [(Text, JExpr)]
                    -> Handler b b JStat
                    -> HeistT (Handler b b) (Attrs, Children)
ajaxElemWithHandler jsParams h = do
  X.Element _ as cs <- getParamNode
  handlerId <- addAjaxCallback h
  children  <- runNodeList cs
  let call = ajaxCallWithParams $ (hidAsText handlerId, Js.null) : jsParams
  return (addAttrs [("onclick", Js.showAsText call)] as, children)

ajaxElemWithParser :: HasAjax b =>
                      (B.ByteString -> Maybe a)
                   -> JExpr
                   -> ButtonHandler b a
                   -> HeistT (Handler b b) (Attrs, Children)
ajaxElemWithParser parse jsExpr process = do
  exprName <- liftAjax newRandomId
  ajaxElemWithHandler [(exprName, jsExpr)] $ do
                                            expr <- getTextRqParam exprName
                                            process $ expr >>= parse

ajaxElem :: (HasAjax b, Read a) =>
            JExpr
         -> ButtonHandler b a
         -> HeistT (Handler b b) (Attrs, Children)
ajaxElem = ajaxElemWithParser $ readMay . B.unpack

ajaxJsonElem :: (HasAjax b, FromJSON a) =>
                JExpr
             -> ButtonHandler b a
             -> HeistT (Handler b b) (Attrs, Children)
ajaxJsonElem jsonExpr = ajaxElemWithParser (decode . lazyFromStrict)
                        [jmacroE|JSON.stringify(`(jsonExpr)`)|]

ajaxElem_ :: HasAjax b => Handler b b JStat -> HeistT (Handler b b) (Attrs, Children)
ajaxElem_ = ajaxElemWithHandler []

------------------------------------------------------------------------------
-- Buttons
------------------------------------------------------------------------------

toButton :: HeistT (Handler b b) (Attrs, Children) -> Splice (Handler b b)
toButton = fmap $ pure . uncurry (X.Element "button")

ajaxButton :: (HasAjax b, Read a) =>
              JExpr -> ButtonHandler b a -> Splice (Handler b b)
ajaxButton j h = toButton $ ajaxElem j h

ajaxButton_ :: HasAjax b => Handler b b JStat -> Splice (Handler b b)
ajaxButton_ h = toButton $ ajaxElem_ h

ajaxJsonButton :: (HasAjax b, FromJSON a) =>
                  JExpr -> ButtonHandler b a -> Splice (Handler b b)
ajaxJsonButton j h = toButton $ ajaxJsonElem j h

------------------------------------------------------------------------------
-- Anchors
------------------------------------------------------------------------------

toAnchor :: HeistT (Handler b b) (Attrs, Children) -> Splice (Handler b b)
toAnchor = fmap $ \(as,cs) -> [X.Element "a" (addAttrs [("href", "javascript://")] as) cs]

ajaxAnchor :: (HasAjax b, Read a) =>
              JExpr -> ButtonHandler b a -> Splice (Handler b b)
ajaxAnchor j h = toAnchor $ ajaxElem j h

ajaxAnchor_ :: HasAjax b => Handler b b JStat -> Splice (Handler b b)
ajaxAnchor_ h = toAnchor $ ajaxElem_ h

ajaxJsonAnchor :: (HasAjax b, FromJSON a) =>
                  JExpr -> ButtonHandler b a -> Splice (Handler b b)
ajaxJsonAnchor j h = toAnchor $ ajaxJsonElem j h
