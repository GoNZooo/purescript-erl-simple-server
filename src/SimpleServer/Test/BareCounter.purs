module SimpleServer.Test.BareCounter
  ( startLink
  , increment
  , currentCount
  , startPhase1Cast
  , getPid
  , shutdown
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Erl.Atom (Atom)
import Erl.Atom as Atom
import Erl.Process (Process, ProcessM)
import Erl.Process as Process
import Pinto (RegistryName(..), StartLinkResult)
import SimpleServer.Bare (InitValue, ProcessReference(..), ReturnValue, ServerPid, StopReason(..))
import SimpleServer.Bare as SimpleServer
import SimpleServer.Test.BareCounter.Types
  ( Arguments
  , Continue(..)
  , Message(..)
  , Pid
  , State
  , Stop(..)
  )

serverName :: RegistryName Pid
serverName = "SimpleServer.Test.BareCounter" # Atom.atom # Local

startLink :: Arguments -> Effect (StartLinkResult (ServerPid Message State Continue Stop))
startLink arguments = do
  SimpleServer.startLink arguments
    { name: Just serverName
    , init
    , handleInfo
    , handleContinue
    , terminate
    }

init :: Arguments -> ProcessM Message (InitValue State Continue Stop)
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

shutdown :: Process Atom -> Atom -> Effect Unit
shutdown pid stopAtom = SimpleServer.cast (NameReference serverName) \state ->
  pure $ SimpleServer.stop (StopShutdown $ Just $ StopWithAtom pid stopAtom) state

startPhase1Cast :: Effect Unit
startPhase1Cast = SimpleServer.cast (NameReference serverName) \state ->
  pure $ SimpleServer.continue Phase1 state

handleInfo :: Message -> State -> ProcessM Message (ReturnValue State Continue Stop)
handleInfo StartPhase1 state =
  state # SimpleServer.continue Phase1 # pure

handleContinue :: Continue -> State -> ProcessM Message (ReturnValue State Continue Stop)
handleContinue Phase1 state =
  state { count = state.count * 2 } # SimpleServer.continue Phase2 # pure
handleContinue Phase2 state =
  state { count = state.count + 1 } # SimpleServer.noReply # pure

terminate :: StopReason Stop -> State -> ProcessM Message Unit
terminate (StopShutdown (Just (StopWithAtom pid stopAtom))) _state = do
  liftEffect $ Process.send pid stopAtom
terminate (StopShutdown Nothing) _state = do
  pure unit
terminate StopNormal _state = do
  pure unit
terminate (StopOther _) _state = do
  pure unit
