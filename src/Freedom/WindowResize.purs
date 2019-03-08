module Freedom.WindowResize where

import Prelude

import Control.Monad.Free.Trans (FreeT)
import Effect.Aff (Aff, launchAff_)
import Freedom.Subscription (Subscription, subscription)
import Web.Event.Event (EventType(..))
import Web.Event.EventTarget (addEventListener, eventListener)
import Web.HTML (window)
import Web.HTML.Window (toEventTarget)

windowResize
  :: forall f state
   . Functor (f state)
  => FreeT (f state) Aff Unit
  -> Subscription f state
windowResize onResizeWindow =
  subscription \transform -> do
    listener <- eventListener $ const $ launchAff_ $ transform onResizeWindow
    window <#> toEventTarget >>= addEventListener resize listener false
  where
    resize = EventType "resize"
