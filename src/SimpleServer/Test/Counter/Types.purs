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
