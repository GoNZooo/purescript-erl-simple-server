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
module MyProject.Counter
  ( startLink
  , increment
  , currentCount
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Erl.Atom as Atom
import Erl.Process (Process, ProcessM)
import Pinto (RegistryName(..), StartLinkResult)
import MyProject.Counter.Types (Arguments, Message(..), State, Pid)
import SimpleServer.GenServer (InitValue, ProcessReference(..), ReturnValue)
import SimpleServer.GenServer as SimpleServer

serverName :: RegistryName Pid
serverName = "MyProject.Counter" # Atom.atom # Local

startLink :: Arguments -> Effect (StartLinkResult (Process Message))
startLink arguments = do
  SimpleServer.startLink arguments { name: Just serverName, init, handleInfo }

init :: Arguments -> ProcessM Message (InitValue State)
init { initialCount } = { count: initialCount } # SimpleServer.initOk # pure

increment :: Effect Unit
increment = SimpleServer.cast (NameReference serverName) \state ->
  pure $ SimpleServer.noReply $ state { count = state.count + 1 }

currentCount :: Effect Int
currentCount = SimpleServer.call (NameReference serverName) \_from state ->
  pure $ SimpleServer.reply state state.count

handleInfo :: Message -> State -> ProcessM Message (ReturnValue State)
handleInfo NoOp state = state # SimpleServer.noReply # pure
```

#### Types file

```purescript
module MyProject.Counter.Types
  ( Message(..)
  , Pid
  , State
  , Arguments
  ) where

import SimpleServer.GenServer (ServerPid)

data Message = NoOp

type State = { count :: Int }

type Arguments = { initialCount :: Int }

type Pid = ServerPid Message State
```

## Features not implemented (yet)

#### `handleContinue`

This is probably going to be implemented in time, but it's just not a priority at the moment. When
added it will be a lot like `handleInfo` in that it's passed in at `startLink` time in the spec
and it will have its type checked so we can only handle valid `Continue` values.

Made with [`purerl`](https://github.com/purerl/purerl).
