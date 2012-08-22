snaplet-liftajax
================

This Snaplet provides splices for AJAX-enabled forms and buttons,
modeled off of AJAX elements in the [Lift web
framework](http://liftweb.net/) for Scala.  A nice feature of Lift is
the ability to associate server-side callbacks directly with html
elements, abstracting over the details of writing the appropriate
client-side javascript and matching server-side routes.  More detail
is given in the book [Exploring
Lift](http://exploring.liftweb.net/master/index-11.html).

Example
-------

Here's an example of an ajax form using the [digestive-functors
package](http://hackage.haskell.org/package/digestive-functors/).
We'll write a form to parse and add two integers server-side.  If
everything goes well, we alert the user with the result.  If not, we
show an alert with the errors.

First, some imports
```haskell
import           Control.Applicative
import           Data.Monoid
import           Data.Text       (Text)
import qualified Data.Text as T
import           Language.Javascript.JMacro
import           Snap.Snaplet.LiftAjax
import qualified Snap.Snaplet.LiftAjax.Js as Js
import           Text.Digestive
import           Text.Digestive.Heist
import           Text.Templating.Heist
import           Application
```

Now, define a form to add two integers
```haskell
addInts :: Monad m => Form Text m Int
addInts = (+)
          <$> "x" .: stringRead "x must be an integer" Nothing
          <*> "y" .: stringRead "y must be an integer" Nothing
```

To this form, associate a function `process` which takes the result
(or an error) and returns javascript to be executed client-side.
```haskell
addIntsSplice :: Splice AppHandler
addIntsSplice = ajaxFormWithSplices digestiveSplices addInts process
    where
      process :: Either (View Text) Int -> AppHandler JStat
      process (Right z) = return $ Js.alert $ "the sum is " <> T.pack (show z)
      process (Left v)  = return $ Js.alert $ T.intercalate ", " $ childErrors "" v
```

If we bind `addIntsSplice` to the tag `<addIntsForm>`, then our heist
template would look like this:
```html
<addIntsForm class="classes for styling">
x = <dfInputText ref="x" />,
y = <dfInputText ref="y" />
<input type="submit" value="Add" />
</addIntsForm>
```

Setup
-----

Add the snaplet to your application's state, and define a `HasAjax`
instance:

```haskell
data App = App
    { _heist :: Snaplet (Heist App)
    , _ajax  :: Snaplet (Ajax App)
    }

makeLens ''App

instance HasHeist App where
    heistLens = subSnaplet heist

instance HasAjax App where
    ajaxLens = ajax

type AppHandler = Handler App App
```

Call nestSnaplet appropriately in your application's initializer:

```haskell
app = makeSnaplet "app" "An snaplet example application." Nothing $ do
    h <- nestSnaplet "" heist $ heistInit "templates"
    a <- nestSnaplet "ajax" ajax $ ajaxInit defaultAjaxState
    addRoutes [("/", serveDirectory "static")]
    addSplices [("addIntsForm", liftHeist addIntsSplice)]
    return $ App h a
```

Add jquery and liftAjax.js to your site's header (and place the files
where they'll be served):

```html
<script type="text/javascript" src="jquery-1.7.2.min.js"></script>
<script type="text/javascript" src="liftAjax.js"></script>
```

Add the `<ajaxFooter />` just before your site's `</body>` tag.

Notes
-----

- Currently, [Jmacro](http://hackage.haskell.org/package/jmacro) is
  used for client-side javascript, but this package can be easily
  adapted to work with some other way of generating javascript.  It
  would be nice to see if there's a natural way to incorporate a more
  haskelly solution like [Fay](http://fay-lang.org/).

- liftAjax.js is taken almost verbatim from the lift web framework.  I
  have intentionally made only minimal modifications.

- the LiftAjax snaplet assigns each page a unique pageId, and keeps a
  map from pageIds to ajax handlers.  In order to decide when to
  garbage collect these handlers, it also keeps track of "heartbeats"
  from each open page -- these are ajax calls automatically sent every
  minute or so (configurable in `ajaxInit`) to say "I'm still open!".
  A background process continually checks for pages with old
  heartbeats and discards their ajax handlers.

- Since this snaplet is still in development, its behavior may not be
  completely consistent with Lift's.
