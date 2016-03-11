---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------
-- | 
-- | Module : Components tutorial
-- | Creator: Xiao Ling
-- | Created: 3/8/2016
-- | Source : https://github.com/paf31/purescript-thermite
-- |          http://www.parsonsmatt.org/2015/10/03/elm_vs_purescript.html
-- |          https://github.com/ethul/purescript-react-example     <- project structure
-- |          https://github.com/paf31/try-thermite/blob/gh-pages/staging/src/Thermite/Try.purs
-- | Workflow: 
-- |          pulp init   - create project folder, with bower.json
-- |          npm  init   - create package.json
-- |          bower install --save [PACKAGE] to note dependencies in bower.json
-- |          npm   install --save [PACKAGE] to note dependencies in package.json
-- |          Now build app.js file,  instead of pulp browserfy, do:
-- |              rm -r output
-- |              pulp build --to browserify-entry-point.js
-- |              browserify browserify-entry-point.js > app.js
-- |          Finally run development server. But instead of pulp server do: 
-- |              serve (installed by  npm install -g serve)
-- |         
-- |          The problem with this approach is that there's no live pushing of updates
-- | 
---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------

module Utils where

import Prelude

import Control.Monad
import Control.Monad.Eff
import Control.Monad.Eff.Console

import qualified React as R
import qualified React.DOM as R
import qualified Thermite as T

import qualified DOM as DOM
import qualified DOM.HTML as DOM
import qualified DOM.HTML.Types as DOM
import qualified DOM.HTML.Window as DOM
import qualified DOM.Node.ParentNode as DOM

import Unsafe.Coerce
import Data.Maybe.Unsafe
import Data.Nullable (toMaybe)


{----------------------------------------------------------------
     Default main
----------------------------------------------------------------}

-- * note :   void :: forall f a. Functor f => f a -> f ()
defaultMain :: forall state action eff. 
                      T.Spec _ state _ action -> state -> Eff (dom :: DOM.DOM | eff) Unit
defaultMain spec state0 = void do
  let component = T.createClass spec state0
  win       <- DOM.window
  doc       <- DOM.document win
  container <- fromJust <<< toMaybe <$> DOM.querySelector "#app" (DOM.htmlDocumentToParentNode doc)
  ReactDOM.render (R.createFactory component {}) container


{----------------------------------------------------------------
    Other
----------------------------------------------------------------}

-- * haskell `undefined`
hole :: forall a. a
hole = unsafeCoerce unit



















