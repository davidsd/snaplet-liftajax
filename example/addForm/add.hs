{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

{-

Author: Henry Laxen (http://nadineloveshenry.com/)

1. Obviously you have already unpacked this file in a directory.
2. Now compile with ghc as follows:
   ghc --make add.hs
3. Now run ./add
4. Go to your browser and enter the url:
   http://localhost.localdomain:8000/ajax
5. The rest is silence      

-}

import Control.Applicative ( Applicative((<*>)), (<$>) )
import Data.Monoid ( (<>) )
import Data.Text ( Text )
import qualified Data.Text as T ( pack, intercalate )
import Language.Javascript.JMacro ( JStat )
import Snap
    ( Snaplet,
      Handler,
      SnapletInit,
      defaultConfig,
      subSnaplet,
      serveSnaplet,
      nestSnaplet,
      makeSnaplet,
      addRoutes,
      makeLens )
import Snap.Snaplet.Heist
    ( Heist, HasHeist(..), liftHeist, heistInit, addSplices )
import Snap.Util.FileServe ( serveDirectory )
import Snap.Snaplet.LiftAjax.Js ( alert )
import Snap.Snaplet.LiftAjax.Splice    ( formWithSplices )
import Text.Digestive ( Form, View, childErrors, (.:), stringRead )
import Text.Digestive.Heist ( digestiveSplices )
import Text.Templating.Heist ( Splice )
import Snap.Snaplet.LiftAjax
    ( Ajax, HasAjax(..), defaultAjaxState, ajaxInit )

type AppHandler = Handler App App

addInts :: Monad m => Form Text m Int
addInts = (+)
          <$> "x" .: stringRead "x must be an integer" Nothing
          <*> "y" .: stringRead "y must be an integer" Nothing

addIntsSplice :: Splice AppHandler
addIntsSplice = formWithSplices digestiveSplices addInts process
    where
      process :: Either (View Text) Int -> AppHandler JStat
      process (Right z) = return $ alert $ "the sum is " <> T.pack (show z)
      process (Left v)  = return $ alert $ T.intercalate ", " $ childErrors "" v

data App = App
    { _heist :: Snaplet (Heist App)
    , _ajax  :: Snaplet (Ajax App)
    }
makeLens ''App

instance HasHeist App where
    heistLens = subSnaplet heist

instance HasAjax App where
    ajaxLens = ajax


app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do
    h <- nestSnaplet "" heist $ heistInit "templates"
    a <- nestSnaplet "ajax" ajax $ ajaxInit defaultAjaxState
    addRoutes [("/", serveDirectory "static")]
    addSplices [("addIntsForm", liftHeist addIntsSplice)]
    return $ App h a

main :: IO ()
main = serveSnaplet defaultConfig app
