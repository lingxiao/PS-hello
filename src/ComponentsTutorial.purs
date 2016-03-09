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

module ComponentsTutorial where

import Prelude

import Data.Lens
import Data.Tuple
import Data.Either
import qualified Data.List as L

import Control.Monad
import Control.Monad.Eff
import Control.Monad.Eff.Console

import qualified Thermite as T
import qualified React.DOM.Props as RP
import qualified React.DOM as R
import qualified React as R

import qualified DOM as DOM
import qualified DOM.HTML as DOM
import qualified DOM.HTML.Types as DOM
import qualified DOM.HTML.Window as DOM
import qualified DOM.Node.ParentNode as DOM

import Data.Maybe.Unsafe
import Data.Nullable (toMaybe)


-- * function synonym
(~>) :: forall eff state props action. 
        Thermite.Render state props action     -> 
        T.PerformAction eff state props action -> 
        Thermite.Spec eff state props action
(~>) = flip T.simpleSpec

{-------------------------------------------------------------------
    Application types
-------------------------------------------------------------------}

-- * a thermite component is just some state
type State     = Int

-- * a action over state
data Action    = Incr | Decr

-- * App state
type AppState  = Tuple State State

-- * Applicaton action
type AppAction = Either Action Action


{-------------------------------------------------------------------
    Views - note all view names are of form FOO'
-------------------------------------------------------------------}

-- * This view has emits signals for listeners
counter' :: T.Render State _ Action
counter' go _ state _ = 
  [ R.p [ RP.className "input-group" ] 
          [ R.span [ RP.className "input-group-btn" ] 
                   [ R.button [ RP.className "btn btn-default"
                              , RP.onClick \_ -> go Decr
                              ]
                              [ R.text "-" ]
                   ] 
          , R.input [ RP.className "form-control"
                    , RP._type "text"
                    , RP.disabled true
                    , RP.value (show state)
                    ] []
          , R.span [ RP.className "input-group-btn" ] 
                   [ R.button [ RP.className "btn btn-default"
                              , RP.onClick \_ -> go Incr
                              ]
                              [ R.text "+" ]
                   ] 
          ]
  ]

header' :: T.Render _ _ _   
header' _ _ _ _ = [ R.h1' [ R.text "This is a header" ] ]

footer' :: T.Render _ _ _   
footer' _ _ _ _ = [ R.p'  [ R.text "This is the footer " ]]
 
{-------------------------------------------------------------------
    Views + Controllers - note all view + controller have name FOO
-------------------------------------------------------------------}

counter :: T.Spec _ State _ _ 
counter = counter' ~> controller
    where 
        controller :: T.PerformAction _ State _ Action
        controller Incr _ _  go = go $ \s -> s + 1
        controller Decr _ _  go = go $ \s -> s - 1

header :: T.Spec _ _ _ _
header = header' ~> T.defaultPerformAction 

-- The footer component
footer :: T.Spec _ _ _ _
footer = footer' ~> T.defaultPerformAction 


{-------------------------------------------------------------------
    Application

    Now, create a specification for a component consisting of two counters.

    The "focus" combinator changes the state type of a component
    by looking at a small part of a larger component's state, using a _Lens_.

    Similarly, we can embed an action types inside the actions of a
    larger component, by using a _Prism_.

    Common lenses include _1 and _2 (representing the two sides of a Tuple),
    and common prisms include _Left and _Right, representing the two constructors 
    of the Either type constructor.

-------------------------------------------------------------------}

-- * Note how Thermite components are monoids
app :: T.Spec _ AppState _ AppAction
app = header <> T.focus _1 _Left counter <> T.focus _2 _Right counter <> footer


-- * every component needs an initial state
state0 :: AppState
state0 = Tuple 0 100


-- * main
main :: forall m. Eff (dom :: DOM.DOM | m ) Unit
main = defaultMain app state0

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






















