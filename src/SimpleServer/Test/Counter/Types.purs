module SimpleServer.Test.Counter.Types
  ( Message(..)
  , Continue(..)
  , Pid
  , State
  , Arguments
  ) where

import SimpleServer.GenServer (ServerPid)

data Message = StartPhase1

data Continue
  = Phase1
  | Phase2

type State = { count :: Int }

type Arguments = { initialCount :: Int }

type Pid = ServerPid Message State Continue
