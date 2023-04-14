module SimpleServer.GenServer
  ( startLink
  , cast
  , call
  , module SimpleServer.Types
  , module SimpleServer.Utilities
  ) where

import Prelude

import Effect (Effect)
import Erl.Process (Process, ProcessM)
import Pinto.Types (StartLinkResult)
import SimpleServer.Types
  ( InitValue
  , ProcessReference(..)
  , Reply
  , ReturnValue
  , ServerPid
  , StartLinkArguments
  , StopReason(..)
  , continue
  , hibernate
  , initContinue
  , initError
  , initOk
  , initHibernate
  , noReply
  , stop
  , reply
  , replyAndStop
  , replyAndContinue
  , replyAndHibernate
  )
import SimpleServer.Utilities (self, sendSelf)

startLink
  :: forall arguments message state continue stop
   . arguments
  -> StartLinkArguments arguments message state continue stop
  -> Effect (StartLinkResult (ServerPid message state continue stop))
startLink startArguments arguments = do
  startLink_ startArguments arguments

foreign import startLink_
  :: forall arguments message state continue stop
   . arguments
  -> StartLinkArguments arguments message state continue stop
  -> Effect (StartLinkResult (ServerPid message state continue stop))

foreign import cast
  :: forall message state continue stop
   . ProcessReference message state continue stop
  -> (state -> ProcessM message (ReturnValue state continue stop))
  -> Effect Unit

foreign import call
  :: forall message state a continue stop
   . ProcessReference message state continue stop
  -> ((Process a) -> state -> ProcessM message (Reply a state continue stop))
  -> Effect a
