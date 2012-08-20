{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Snap.Snaplet.LiftAjax.Js where

------------------------------------------------------------------------------
import           Blaze.ByteString.Builder
import           Data.ByteString            (ByteString)
import qualified Data.ByteString.Char8      as B
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Language.Javascript.JMacro
import           Snap.Core
import qualified Text.PrettyPrint           as PP
import qualified Text.XmlHtml               as X
------------------------------------------------------------------------------

instance ToJExpr Text where
    toJExpr = toJExpr . T.unpack

instance ToJExpr [X.Node] where
    toJExpr = toJExpr . B.unpack . toByteString . X.renderHtmlFragment X.UTF8

write :: (JsToDoc a, JMacro a, MonadSnap m) => a -> m ()
write = writeBS . showAsBS

showAsBS :: (JsToDoc a, JMacro a) => a -> ByteString
showAsBS = B.pack . PP.renderStyle (PP.style { PP.mode = PP.OneLineMode }) . renderJs

showAsText :: (JsToDoc a, JMacro a) => a -> Text
showAsText = T.pack . PP.renderStyle (PP.style { PP.mode = PP.OneLineMode }) . renderJs

noop :: JStat
noop = [jmacro|$.noop();|];

alert :: Text -> JStat
alert msg = [jmacro|alert(`(msg)`);|];
