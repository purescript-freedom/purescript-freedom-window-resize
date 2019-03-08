module Main where

import Prelude

import Freedom as Freedom
import Freedom.Markup as H
import Freedom.WindowResize (windowResize)
import Freedom.Subscription (Subscription)
import Freedom.TransformF.Simple (VQueryF, transformF, reduce)
import Freedom.VNode (VNode)
import Effect (Effect)
import Effect.Class (liftEffect)
import Web.HTML (window)
import Web.HTML.Window as W

type WindowSize =
  { innerWidth :: Int
  , innerHeight :: Int
  }

type State = WindowSize

type Sub = Subscription VQueryF State

type Html = VNode VQueryF State

main :: Effect Unit
main = do
  initialState <- windowSize
  Freedom.run
    { selector: "#app"
    , initialState
    , subscriptions: [ windowResize' ]
    , transformF
    , view
    }

windowSize :: Effect WindowSize
windowSize =
  { innerWidth: _, innerHeight: _ }
    <$> (window >>= W.innerWidth)
    <*> (window >>= W.innerHeight)

windowResize' :: Sub
windowResize' = windowResize do
  state <- liftEffect $ windowSize
  reduce $ const state

view :: State -> Html
view { innerWidth, innerHeight } =
  H.el $ H.div # H.kids
    [ H.el $ H.h1 # H.kids [ H.t "WindowResize Demo" ]
    , H.el $ H.p # H.kids [ H.t $ "Width: " <> show innerWidth ]
    , H.el $ H.p # H.kids [ H.t $ "Height: " <> show innerHeight ]
    ]
