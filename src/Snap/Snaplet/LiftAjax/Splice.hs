{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Snap.Snaplet.LiftAjax.Splice where

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

type AjaxCallback b = Handler b b JStat

type FormCallback b v a = Either (View v) a -> AjaxCallback b
type ButtonCallback b a = Maybe a -> AjaxCallback b

type Attrs = [(Text, Text)]

-- Does not override existing attributes
addAttrs :: Attrs  -- ^ Original attributes
         -> Attrs  -- ^ Attributes to add
         -> Attrs  -- ^ Resulting attributes
addAttrs = unionBy (on (==) fst)

mapAttrs :: (Attrs -> Attrs) -> X.Node -> X.Node
mapAttrs f (X.Element t a c) = X.Element t (f a) c
mapAttrs _ n = n

lazyFromStrict :: B.ByteString -> LB.ByteString
lazyFromStrict = LB.fromChunks . pure

liftAjax :: HasAjax b => AjaxHandler b a -> HeistT (Handler b b) a
liftAjax = lift . withTop ajaxLens

addAjaxCallback :: HasAjax b => AjaxCallback b -> AjaxHandler b HandlerId
addAjaxCallback = addCallback . (>>= Js.write)

------------------------------------------------------------------------------
-- Ajax Calls
------------------------------------------------------------------------------

ajaxCall :: JExpr -> JStat
ajaxCall e = [jmacro| liftAjax.lift_ajaxHandler(`(e)`, null, null, 'javascript');
                      return false; |]

encodeParams :: [(Text, JExpr)] -> JExpr
encodeParams = collect . map pair
    where
      pair (a,b) = [jmacroE|`(a)`+"="+encodeURIComponent(`(b)`)|]
      collect = foldl1Def Js.null (\x xs -> [jmacroE|`(x)`+"&"+`(xs)`|])

callHandlerWithParams :: HasAjax b =>
                         [(Text, JExpr)]
                      -> AjaxCallback b
                      -> AjaxHandler b JStat
callHandlerWithParams jsParams h = do
  handlerId <- addAjaxCallback h
  return $ ajaxCall $ encodeParams $ (hidAsText handlerId, Js.null) : jsParams

callHandler :: HasAjax b => AjaxCallback b -> AjaxHandler b JStat
callHandler = callHandlerWithParams []

callWithParser :: HasAjax b =>
                  (B.ByteString -> Maybe a)
               -> JExpr
               -> ButtonCallback b a
               -> AjaxHandler b JStat
callWithParser parse jsExpr process = do
  exprName <- newRandomId
  callHandlerWithParams [(exprName, jsExpr)] $ do
                                        expr <- getTextRqParam exprName
                                        process $ expr >>= parse

callWithRead :: (HasAjax b, Read a) =>
                JExpr
             -> ButtonCallback b a
             -> AjaxHandler b JStat
callWithRead = callWithParser $ readMay . B.unpack

callWithJson :: (HasAjax b, FromJSON a) =>
                JExpr
             -> ButtonCallback b a
             -> AjaxHandler b JStat
callWithJson jsonExpr = callWithParser (decode . lazyFromStrict)
                        [jmacroE|JSON.stringify(`(jsonExpr)`)|]

------------------------------------------------------------------------------
-- Forms
------------------------------------------------------------------------------

formWithCallback :: HasAjax b => AjaxCallback b -> Splice (Handler b b)
formWithCallback h = do
  X.Element _ attrs cs <- getParamNode
  formId    <- liftAjax newRandomId
  handlerId <- liftAjax $ addAjaxCallback h
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
     -> FormCallback b v a
     -> Splice (Handler b b)
form name f process = formWithCallback $ do
                        (view, result) <- runForm name f
                        process (maybe (Left view) Right result)

formWithSplices :: HasAjax b =>
                   (View v -> [(Text, Splice (Handler b b))])
                -> Form v (Handler b b) a
                -> FormCallback b v a
                -> Splice (Handler b b)
formWithSplices splices f process = do
  name <- liftAjax newRandomId
  view <- lift $ getForm name f
  localTS (bindSplices $ splices view) $ form name f process

------------------------------------------------------------------------------
-- Elements
------------------------------------------------------------------------------

onEvent :: Functor m => Text -> m JStat -> m Attrs
onEvent e = fmap $ \js -> [(e, Js.showAsText js)]

onClick :: Functor m => m JStat -> m Attrs
onClick = onEvent "onclick"

runElem :: Monad m => Text -> Attrs -> Splice m
runElem tag attrs = do
  X.Element _ as cs <- getParamNode
  cs' <- runNodeList cs
  return [X.Element tag (addAttrs attrs as) cs']

withAction :: HasAjax b =>
              AjaxHandler b Attrs
           -> Splice (Handler b b)
           -> Splice (Handler b b)
withAction ajaxAttrs splice = do
  nodes <- splice
  as <- liftAjax ajaxAttrs
  return $ map (mapAttrs $ addAttrs as) nodes

------------------------------------------------------------------------------
-- Buttons
------------------------------------------------------------------------------

ajaxButton :: HasAjax b => AjaxHandler b Attrs -> Splice (Handler b b)
ajaxButton = flip withAction $ runElem "button" []

button :: HasAjax b => AjaxCallback b -> Splice (Handler b b)
button h = ajaxButton $ onClick $ callHandler h

readButton :: (HasAjax b, Read a) =>
              JExpr -> ButtonCallback b a -> Splice (Handler b b)
readButton j h = ajaxButton $ onClick $ callWithRead j h

jsonButton :: (HasAjax b, FromJSON a) =>
              JExpr -> ButtonCallback b a -> Splice (Handler b b)
jsonButton j h = ajaxButton $ onClick $ callWithJson j h

------------------------------------------------------------------------------
-- Anchors
------------------------------------------------------------------------------

ajaxAnchor :: HasAjax b => AjaxHandler b Attrs -> Splice (Handler b b)
ajaxAnchor = flip withAction $ runElem "a" [("href", "javascript://")]

anchor :: HasAjax b => AjaxCallback b -> Splice (Handler b b)
anchor h = ajaxAnchor $ onClick $ callHandler h

readAnchor :: (HasAjax b, Read a) =>
              JExpr -> ButtonCallback b a -> Splice (Handler b b)
readAnchor j h = ajaxAnchor $ onClick $ callWithRead j h

jsonAnchor :: (HasAjax b, FromJSON a) =>
              JExpr -> ButtonCallback b a -> Splice (Handler b b)
jsonAnchor j h = ajaxAnchor $ onClick $ callWithJson j h

------------------------------------------------------------------------------
-- Inputs
------------------------------------------------------------------------------

callChecked :: HasAjax b => (Bool -> AjaxCallback b) -> AjaxHandler b JStat
callChecked = callWithRead [jmacroE|this.checked?"True":"False"|] . maybe pass

checkbox :: HasAjax b => (Bool -> AjaxCallback b) -> Splice (Handler b b)
checkbox h = (withAction $ onClick $ callChecked h) freshCheckbox
    where
      freshCheckbox = runElem "input" [("type", "checkbox")]
