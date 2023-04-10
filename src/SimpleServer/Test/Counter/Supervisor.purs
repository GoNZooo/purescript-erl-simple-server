module SimpleServer.Test.Counter.Supervisor
  ( startLink
  , startChild
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..), Seconds(..))
import Effect (Effect)
import Erl.Atom as Atom
import Erl.Process (Process)
import Pinto (RegistryName(..), RegistryReference(..), StartLinkResult)
import Pinto.Supervisor
  ( ChildShutdownTimeoutStrategy(..)
  , ChildType(..)
  , RestartStrategy(..)
  , crashIfChildNotRunning
  )
import Pinto.Supervisor.SimpleOneForOne as Supervisor
import SimpleServer.Test.Counter as Counter
import SimpleServer.Test.Counter.Types as CounterTypes

type SupervisorType = Supervisor.SupervisorType CounterTypes.Arguments CounterTypes.Pid
type Pid = Supervisor.SupervisorPid CounterTypes.Arguments CounterTypes.Pid

name :: RegistryName SupervisorType
name = "SimpleServer.Test.Counter.Supervisor" # Atom.atom # Local

startLink :: Effect (StartLinkResult Pid)
startLink = do
  let
    childType = Worker
    intensity = 5
    period = Seconds 10.0
    restartStrategy = RestartTransient
    start = Counter.startLink
    shutdownStrategy = 5000.0 # Milliseconds # ShutdownTimeout
    init = pure { childType, intensity, period, restartStrategy, start, shutdownStrategy }
  Supervisor.startLink (Just name) init

startChild :: CounterTypes.Arguments -> Effect CounterTypes.Pid
startChild arguments = do
  crashIfChildNotRunning <$> Supervisor.startChild (ByName name) arguments
