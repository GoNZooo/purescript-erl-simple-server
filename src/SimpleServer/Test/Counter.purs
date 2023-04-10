module SimpleServer.Test.Counter
  ( startLink
  , increment
  , currentCount
  , startPhase1Cast
  , getPid
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Erl.Atom as Atom
import Erl.Process (Process, ProcessM)
import Pinto (RegistryName(..), StartLinkResult)
import SimpleServer.GenServer (InitValue, ProcessReference(..), ReturnValue)
import SimpleServer.GenServer as SimpleServer
import SimpleServer.Test.Counter.Types (Arguments, Continue(..), Message(..), Pid, State)

serverName :: RegistryName Pid
serverName = "SimpleServer.Test.Counter" # Atom.atom # Local

startLink :: Arguments -> Effect (StartLinkResult (Process Message))
startLink arguments = do
  SimpleServer.startLink arguments { name: Just serverName, init, handleInfo, handleContinue }

init :: Arguments -> ProcessM Message (InitValue State Continue)
init { initialCount } = do
  { count: initialCount } # SimpleServer.initContinue Phase2 # pure

increment :: Effect Unit
increment = SimpleServer.cast (NameReference serverName) \state ->
  pure $ SimpleServer.noReply $ state { count = state.count + 1 }

currentCount :: Effect Int
currentCount = SimpleServer.call (NameReference serverName) \_from state ->
  pure $ SimpleServer.reply state state.count

getPid :: Effect (Process Message)
getPid = SimpleServer.call (NameReference serverName) \_from state -> do
  pid <- SimpleServer.self
  pure $ SimpleServer.reply state pid

startPhase1Cast :: Effect Unit
startPhase1Cast = SimpleServer.cast (NameReference serverName) \state ->
  pure $ SimpleServer.continue Phase1 state

handleInfo :: Message -> State -> ProcessM Message (ReturnValue State Continue)
handleInfo StartPhase1 state =
  state # SimpleServer.continue Phase1 # pure

handleContinue :: Continue -> State -> ProcessM Message (ReturnValue State Continue)
handleContinue Phase1 state =
  state { count = state.count * 2 } # SimpleServer.continue Phase2 # pure
handleContinue Phase2 state =
  state { count = state.count + 1 } # SimpleServer.noReply # pure
