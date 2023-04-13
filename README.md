# simple-server

A simpler set of bindings to `gen_server` and friends, with less overhead than `Pinto`.

## Motivation

When using `Pinto` for workloads that required a lot of handling in any one given server I noticed
that there was a lot of overhead (~6-7 times that of a Erlang/Elixir version) and so I decided to
recreate the subset of Pinto qualities that I needed in this library:

- Type safety for incoming messages
- Simple name registration for processes
- Known process types (`Process Message`) to provide pids that can be interacted safely with
- Easy-to-use functions for `cast`, `call` and `info` callbacks

## Basic usage

### `gen_server`

An implementation of `gen_server` can be found in `SimpleServer.GenServer`. For a custom
implementation you can also use `SimpleServer.Bare`. Note that this is a simple server loop that
does basically what `gen_server` does but with marginally less overhead (not proven via benchmarks).

In most cases you'll want to use `SimpleServer.GenServer` since it's very likely that `proc_lib`
helps you out in tough spots.

#### Server file

```purescript
module SimpleServer.Test.Counter
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
import SimpleServer.GenServer (InitValue, ProcessReference(..), ReturnValue, StopReason(..))
import SimpleServer.GenServer as SimpleServer
import SimpleServer.Test.Counter.Types (Arguments, Continue(..), Message(..), Pid, State, Stop(..))

serverName :: RegistryName Pid
serverName = "SimpleServer.Test.Counter" # Atom.atom # Local

startLink :: Arguments -> Effect (StartLinkResult Pid)
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
```

#### Types file

```purescript
module SimpleServer.Test.Counter.Types
  ( Message(..)
  , Continue(..)
  , Pid
  , State
  , Arguments
  , Stop(..)
  ) where

import Erl.Atom (Atom)
import Erl.Process (Process)
import SimpleServer.GenServer (ServerPid)

data Stop = StopWithAtom (Process Atom) Atom

data Message = StartPhase1

data Continue
  = Phase1
  | Phase2

type State = { count :: Int }

type Arguments = { initialCount :: Int }

type Pid = ServerPid Message State Continue Stop
```

## Features not implemented (yet)

### Hibernate

Functionality for explicitly hibernating has not been implemented yet.

### Your suggestions

Leave a suggestion for what you need implemented here.
