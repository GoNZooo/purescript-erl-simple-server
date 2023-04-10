module SimpleServer.Utilities
  ( self
  , sendSelf
  ) where

import Data.Unit (Unit)
import Erl.Process (Process, ProcessM)

self :: forall message. ProcessM message (Process message)
self = selfPid

foreign import selfPid :: forall message. ProcessM message (Process message)
foreign import sendSelf :: forall message. message -> ProcessM message Unit
