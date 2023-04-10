module SimpleServer.Bare
  ( startLink
  , cast
  , call
  , module SimpleServer.Utilities
  , module SimpleServer.Types
  ) where

import Prelude

import Effect (Effect)
import Erl.Process (Process, ProcessM)
import Pinto.Types (StartLinkResult)
import SimpleServer.Types
  ( InitValue
  , ProcessReference(..)
  , StopReason(..)
  , Reply
  , ReturnValue
  , ServerPid
  , StartLinkArguments
  , initError
  , initContinue
  , initOk
  , noReply
  , continue
  , reply
  , stop
  )
import SimpleServer.Utilities (self, sendSelf)

startLink
  :: forall arguments message state continue
   . arguments
  -> StartLinkArguments arguments message state continue
  -> Effect (StartLinkResult (Process message))
startLink startArguments arguments = do
  startLink_ startArguments arguments

foreign import startLink_
  :: forall arguments message state continue
   . arguments
  -> StartLinkArguments arguments message state continue
  -> Effect (StartLinkResult (Process message))

foreign import cast
  :: forall message state continue
   . ProcessReference message state continue
  -> (state -> ProcessM message (ReturnValue state continue))
  -> Effect Unit

foreign import call
  :: forall message state a continue
   . ProcessReference message state continue
  -> ((Process a) -> state -> ProcessM message (Reply a state continue))
  -> Effect a
