---------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------
-- | 
-- | Module : Main
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
-- |          Now build app.js file,  instead of pulp browserify, do:
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

module Main where

import Prelude

import Data.Lens
import Data.Maybe
import Data.Tuple
import Data.Either
import qualified Data.List as L

import Control.Monad
import Control.Monad.Eff
import Control.Monad.Eff.Console
import Control.Monad.Aff (Aff, later')

import qualified Thermite as T
import qualified Thermite.Aff as T

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

import Utils


-- * function synonym
(~>) :: forall eff state props action. 
        Thermite.Render state props action     -> 
        T.PerformAction eff state props action -> 
        Thermite.Spec eff state props action
(~>) = flip T.simpleSpec

{-------------------------------------------------------------------
    Application types
-------------------------------------------------------------------}

-- * for each module in application
type Id        = Int
type State     = Int
data Action    = Incr | Decr

-- * for the entire application
type AppState  = L.List State

data AppAction = AddCounter | Item Id Action

{-------------------------------------------------------------------
    Views - note all view names are of form FOO'
-------------------------------------------------------------------}

-- * This view has emits signals for listeners
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

-- * header adds new counters
header' :: T.Render AppState _ AppAction
header' go _ _ _ = [ R.h1' [ R.text "This is the Header" ]
                   , R.p'  [ R.button [ RP.className "btn btn-success"
                                      , RP.onClick \_ -> go AddCounter ]
                             [ R.text "Add Counter" ] 
                           ]
                  ]

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
header = header' ~> (controller state0)
  where
    -- * module initial state
    state0 :: State
    state0  = 100 

    controller :: State -> T.PerformAction _ AppState _ AppAction
    controller s0 AddCounter _ _ update = update $ flip L.snoc s0
    controller _  _          _ _ _      = pure unit

footer :: T.Spec _ _ _ _
footer = footer' ~> T.defaultPerformAction 


{----------------------------------------------------------------------------------
    Application
----------------------------------------------------------------------------------}

-- * Note how Thermite components are monoids
app :: T.Spec _ AppState _ AppAction
app = header <> T.focus id _Item (T.foreach \_ -> counter) <> footer

-- * app initial state
appState0 :: AppState
appState0 = L.Nil

-- * main app
main :: forall m. Eff (dom :: DOM.DOM | m ) Unit
main = defaultMain app appState0

{-------------------------------------------------------------------
  Define our own Prism corresponding to the Item
  data constructor.
-------------------------------------------------------------------}

-- * prism' :: forall s a. (a -> s) -> (s -> Maybe a) 
-- *                        -> (forall p. Choice p => p a a -> p s s)
_Item :: PrismP AppAction (Tuple Id Action)
_Item = prism' (uncurry Item) unwrap
  where
    unwrap (Item i a) = Just (Tuple i a)
    unwrap _          = Nothing

















