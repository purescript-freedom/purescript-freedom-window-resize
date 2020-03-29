module Freedom.WindowResize where

import Prelude

import Effect (Effect)
import Freedom.Store (Query)
import Freedom.UI (Subscription)
import Web.Event.Event (EventType(..))
import Web.Event.EventTarget (addEventListener, eventListener)
import Web.HTML (window)
import Web.HTML.Window (toEventTarget)

windowResize
  :: forall state
   . (Query state -> Effect Unit)
  -> Subscription state
windowResize onResizeWindow query = do
  listener <- eventListener $ const $ onResizeWindow query
  window <#> toEventTarget >>= addEventListener resize listener false
  where
    resize = EventType "resize"
