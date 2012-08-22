{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Snap.Snaplet.LiftAjax.Splice
    ( formWithHandler
    , form
    , formWithSplices
    , elemWithHandler
    , elemWithParser
    , button
    , readButton
    , jsonButton
    , anchor
    , readAnchor
    , jsonAnchor
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

formWithHandler :: HasAjax b => Handler b b JStat -> Splice (Handler b b)
formWithHandler h = do
  X.Element _ attrs cs <- getParamNode
  formId    <- liftAjax newRandomId
  handlerId <- addAjaxCallback h
  children  <- runNodeList cs
  let hidden = X.Element "input" [ ("type", "hidden")
                                 , ("name", hidAsText handlerId)
                                 , ("id",   hidAsText handlerId)
                                 ] []
      f = X.Element "form" (addAttrs [ ("action",   "javascript://")
                                     , ("onsubmit", Js.showAsText sendForm)
                                     , ("id",       formId)
                                     ] attrs) (children ++ [hidden])
      sendForm = ajaxCall [jmacroE|$(`("#"<>formId)`).serialize()|]
  return [f]

form :: HasAjax b =>
        Text
     -> Form v (Handler b b) a
     -> FormHandler b v a
     -> Splice (Handler b b)
form name f process = formWithHandler $ do
                        (view, result) <- runForm name f
                        process (maybe (Left view) Right result)

formWithSplices :: HasAjax b =>
                   (View v -> [(Text, Splice (Handler b b))])
                -> Form v (Handler b b) a
                -> FormHandler b v a
                -> Splice (Handler b b)
formWithSplices splices f process = do
  name <- liftAjax newRandomId
  view <- lift $ getForm name f
  localTS (bindSplices $ splices view) $ form name f process

------------------------------------------------------------------------------
-- Elements
------------------------------------------------------------------------------

ajaxElem :: HasAjax b =>
        [(Text, JExpr)]
     -> Handler b b JStat
     -> HeistT (Handler b b) (Attrs, Children)
ajaxElem jsParams h = do
  X.Element _ as cs <- getParamNode
  handlerId <- addAjaxCallback h
  children  <- runNodeList cs
  let call = ajaxCallWithParams $ (hidAsText handlerId, Js.null) : jsParams
  return (addAttrs [("onclick", Js.showAsText call)] as, children)

elemWithHandler :: HasAjax b => Handler b b JStat -> HeistT (Handler b b) (Attrs, Children)
elemWithHandler = ajaxElem []

elemWithParser :: HasAjax b =>
                  (B.ByteString -> Maybe a)
               -> JExpr
               -> ButtonHandler b a
               -> HeistT (Handler b b) (Attrs, Children)
elemWithParser parse jsExpr process = do
  exprName <- liftAjax newRandomId
  ajaxElem [(exprName, jsExpr)] $ do
    expr <- getTextRqParam exprName
    process $ expr >>= parse

readElem :: (HasAjax b, Read a) =>
            JExpr
         -> ButtonHandler b a
         -> HeistT (Handler b b) (Attrs, Children)
readElem = elemWithParser $ readMay . B.unpack

jsonElem :: (HasAjax b, FromJSON a) =>
            JExpr
         -> ButtonHandler b a
         -> HeistT (Handler b b) (Attrs, Children)
jsonElem jsonExpr = elemWithParser (decode . lazyFromStrict)
                    [jmacroE|JSON.stringify(`(jsonExpr)`)|]

------------------------------------------------------------------------------
-- Buttons
------------------------------------------------------------------------------

toButton :: HeistT (Handler b b) (Attrs, Children) -> Splice (Handler b b)
toButton = fmap $ pure . uncurry (X.Element "button")

button :: HasAjax b => Handler b b JStat -> Splice (Handler b b)
button h = toButton $ elemWithHandler h

readButton :: (HasAjax b, Read a) =>
              JExpr -> ButtonHandler b a -> Splice (Handler b b)
readButton j h = toButton $ readElem j h

jsonButton :: (HasAjax b, FromJSON a) =>
              JExpr -> ButtonHandler b a -> Splice (Handler b b)
jsonButton j h = toButton $ jsonElem j h

------------------------------------------------------------------------------
-- Anchors
------------------------------------------------------------------------------

toAnchor :: HeistT (Handler b b) (Attrs, Children) -> Splice (Handler b b)
toAnchor = fmap $ \(as,cs) -> [X.Element "a" (addAttrs [("href", "javascript://")] as) cs]

anchor :: HasAjax b => Handler b b JStat -> Splice (Handler b b)
anchor h = toAnchor $ elemWithHandler h

readAnchor :: (HasAjax b, Read a) =>
              JExpr -> ButtonHandler b a -> Splice (Handler b b)
readAnchor j h = toAnchor $ readElem j h

jsonAnchor :: (HasAjax b, FromJSON a) =>
              JExpr -> ButtonHandler b a -> Splice (Handler b b)
jsonAnchor j h = toAnchor $ jsonElem j h
