module SimpleServer.Supervisor
  ( startLink
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Time.Duration (Seconds(..))
import Effect (Effect)
import Erl.Atom (atom)
import Erl.Data.List as ErlList
import Pinto.Supervisor (SupervisorPid)
import Pinto.Supervisor as Supervisor
import Pinto.Types (RegistryName(..), StartLinkResult)
import SimpleServer.Supervisor.Helpers as SupervisorHelpers
import SimpleServer.Test.BareCounter as BareCounter
import SimpleServer.Test.Counter as TestCounter

startLink :: Effect (StartLinkResult SupervisorPid)
startLink = Supervisor.startLink (Just $ Local $ atom supervisorName) $ pure supervisorSpec
  where
  supervisorSpec = { childSpecs, flags }
  supervisorName = "SimpleServer.Supervisor"
  childSpecs =
    ErlList.fromFoldable
      [ SupervisorHelpers.worker "SimpleServer.Test.Counter" $
          TestCounter.startLink { initialCount: 0 }
      , SupervisorHelpers.worker "SimpleServer.Test.BareCounter" $
          BareCounter.startLink { initialCount: 0 }
      ]
  flags = { strategy, intensity, period }
  strategy = Supervisor.OneForOne
  intensity = 3
  period = Seconds 5.0

