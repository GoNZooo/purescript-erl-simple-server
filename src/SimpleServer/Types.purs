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

import Prelude

import Data.Maybe (Maybe)
import Erl.Process (Process, ProcessM)
import Erl.Process.Raw (class HasPid, getPid)
import Foreign (Foreign)
import Foreign as Foreign
import Pinto.Types (RegistryName)

type StartLinkArguments arguments message state continue stop =
  { init :: arguments -> ProcessM message (InitValue state continue stop)
  , handleInfo :: message -> state -> ProcessM message (ReturnValue state continue stop)
  , handleContinue :: continue -> state -> ProcessM message (ReturnValue state continue stop)
  , terminate :: StopReason stop -> state -> ProcessM message Unit
  , name :: Maybe (RegistryName (ServerPid message state continue stop))
  }

newtype ServerPid :: forall state' c stop'. Type -> state' -> c -> stop' -> Type
newtype ServerPid message state continue stop = ServerPid (Process message)

instance HasPid (ServerPid message state continue stop) where
  getPid (ServerPid p) = getPid p

data ProcessReference :: forall state' continue' stop'. Type -> state' -> continue' -> stop' -> Type
data ProcessReference message state continue stop
  = PidReference (ServerPid message state continue stop)
  | NameReference (RegistryName (ServerPid message state continue stop))

data InitValue state continue stop
  = SimpleInitOk state
  | SimpleInitContinue state continue
  | SimpleInitStop (StopReason stop) state
  | SimpleInitError Foreign

data ReturnValue state continue stop
  = SimpleNoReply state
  | SimpleContinue continue state
  | SimpleStop (StopReason stop) state

data StopReason stop
  = StopNormal
  | StopShutdown (Maybe stop)
  | StopOther Foreign

data Reply a state continue stop
  = SimpleCallReply a state
  | SimpleCallReplyContinue a state continue
  | SimpleCallStop a (StopReason stop) state

noReply :: forall state continue stop. state -> ReturnValue state continue stop
noReply state = SimpleNoReply state

continue :: forall state continue stop. continue -> state -> ReturnValue state continue stop
continue continue' state = SimpleContinue continue' state

reply :: forall state a continue stop. state -> a -> Reply a state continue stop
reply state reply' = SimpleCallReply reply' state

stop :: forall state continue stop. StopReason stop -> state -> ReturnValue state continue stop
stop reason state = SimpleStop reason state

replyAndStop
  :: forall state a continue stop. state -> StopReason stop -> a -> Reply a state continue stop
replyAndStop state reason reply' = SimpleCallStop reply' reason state

initOk :: forall state continue stop. state -> InitValue state continue stop
initOk state = SimpleInitOk state

initContinue :: forall state continue stop. continue -> state -> InitValue state continue stop
initContinue continue' state = SimpleInitContinue state continue'

initError :: forall state a continue stop. a -> InitValue state continue stop
initError value = SimpleInitError (Foreign.unsafeToForeign value)

