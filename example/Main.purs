module Main where

import Prelude

import Effect (Effect)
import Freedom as Freedom
import Freedom.Markup as H
import Freedom.UI (VNode, Subscription)
import Freedom.WindowResize (windowResize)
import Web.HTML (window)
import Web.HTML.Window as W

type WindowSize =
  { innerWidth :: Int
  , innerHeight :: Int
  }

type State = WindowSize

main :: Effect Unit
main = do
  initialState <- windowSize
  Freedom.run
    { selector: "#app"
    , initialState
    , subscriptions: [ windowResize' ]
    , view
    }

windowSize :: Effect WindowSize
windowSize =
  { innerWidth: _, innerHeight: _ }
    <$> (window >>= W.innerWidth)
    <*> (window >>= W.innerHeight)

windowResize' :: Subscription State
windowResize' = windowResize \query ->
  windowSize >>= const >>> query.reduce

view :: State -> VNode State
view { innerWidth, innerHeight } =
  H.div # H.kids
    [ H.h1 # H.kids [ H.t "WindowResize Demo" ]
    , H.p # H.kids [ H.t $ "Width: " <> show innerWidth ]
    , H.p # H.kids [ H.t $ "Height: " <> show innerHeight ]
    ]
