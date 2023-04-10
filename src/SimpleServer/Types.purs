module SimpleServer.Types
  ( StartLinkArguments(..)
  , ServerPid(..)
  , ProcessReference(..)
  , InitValue
  , ReturnValue
  , Reply
  , StopReason(..)
  , noReply
  , continue
  , reply
  , replyAndStop
  , stop
  , initOk
  , initContinue
  , initError
  ) where

import Data.Maybe (Maybe)
import Erl.Process (Process, ProcessM)
import Erl.Process.Raw (class HasPid, getPid)
import Foreign (Foreign)
import Foreign as Foreign
import Pinto.Types (RegistryName)

type StartLinkArguments arguments message state continue =
  { init :: arguments -> ProcessM message (InitValue state continue)
  , handleInfo :: message -> state -> ProcessM message (ReturnValue state continue)
  , handleContinue :: continue -> state -> ProcessM message (ReturnValue state continue)
  , name :: Maybe (RegistryName (ServerPid message state continue))
  }

newtype ServerPid :: forall s c. Type -> s -> c -> Type
newtype ServerPid message state continue = ServerPid (Process message)

instance HasPid (ServerPid message state continue) where
  getPid (ServerPid p) = getPid p

data ProcessReference :: forall s c. Type -> s -> c -> Type
data ProcessReference message state continue
  = PidReference (ServerPid message state continue)
  | NameReference (RegistryName (ServerPid message state continue))

data InitValue state continue
  = SimpleInitOk state
  | SimpleInitContinue state continue
  | SimpleInitError Foreign

data ReturnValue state continue
  = SimpleNoReply state
  | SimpleContinue continue state
  | SimpleStop StopReason state

data StopReason
  = StopNormal
  | StopShutdown
  | StopOther Foreign

data Reply a state continue
  = SimpleCallReply a state
  | SimpleCallReplyContinue a state continue
  | SimpleCallStop a StopReason state

noReply :: forall state continue. state -> ReturnValue state continue
noReply state = SimpleNoReply state

continue :: forall state continue. continue -> state -> ReturnValue state continue
continue continue' state = SimpleContinue continue' state

reply :: forall state a continue. state -> a -> Reply a state continue
reply state reply' = SimpleCallReply reply' state

stop :: forall state continue. StopReason -> state -> ReturnValue state continue
stop reason state = SimpleStop reason state

replyAndStop :: forall state a continue. state -> StopReason -> a -> Reply a state continue
replyAndStop state reason reply' = SimpleCallStop reply' reason state

initOk :: forall state continue. state -> InitValue state continue
initOk state = SimpleInitOk state

initContinue :: forall state continue. continue -> state -> InitValue state continue
initContinue continue' state = SimpleInitContinue state continue'

initError :: forall state a continue. a -> InitValue state continue
initError value = SimpleInitError (Foreign.unsafeToForeign value)

