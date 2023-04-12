module Test.SimpleServerSpec
  ( main
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import Erl.Atom (Atom)
import Erl.Atom as Atom
import Erl.Kernel.Application as Application
import Erl.Process (Process)
import Erl.Process as Process
import PurerlTest (assertEqual, runSuites, suite, test)
import SimpleServer.Test.BareCounter as BareCounter
import SimpleServer.Test.BareCounter.Types as BareCounterTypes
import SimpleServer.Test.Counter as TestCounter
import SimpleServer.Test.Counter.Types as TestCounterTypes

main :: Effect Unit
main = do
  "simple_server" # Atom.atom # Application.ensureAllStarted # void

  runSuites do
    suite "SimpleServer.Test.Counter" do
      test "goes through correct flow of events" do
        count <- liftEffect TestCounter.currentCount
        count `assertEqual` 1

        liftEffect TestCounter.increment
        count2 <- liftEffect TestCounter.currentCount
        count2 `assertEqual` 2

        -- `startPhase1` goes into a `handle_continue` that should double the current count
        -- and then add 1 to it
        liftEffect TestCounter.startPhase1Cast
        count3 <- liftEffect TestCounter.currentCount
        count3 `assertEqual` 5

        -- we can also send a message to start phase1 and go through the same flow
        counterPid <- liftEffect TestCounter.getPid
        TestCounterTypes.StartPhase1 # Process.send counterPid # liftEffect
        count4 <- liftEffect TestCounter.currentCount
        count4 `assertEqual` 11

        -- shut down the server and wait for `terminate` to be called
        -- `terminate` for the counter process sends back a message with
        -- whichever atom we've specified in `TestCounter.shutdown` so we can
        -- verify that we have indeed run `terminate`
        self <- liftEffect selfPid
        liftEffect $ TestCounter.shutdown self (Atom.atom "our_test_atom")
        result <- liftEffect receiveAtom
        result `assertEqual` Just (Atom.atom "our_test_atom")

    suite "SimpleServer.Test.BareCounter" do
      test "goes through correct flow of events" do
        count <- liftEffect BareCounter.currentCount
        count `assertEqual` 1

        liftEffect BareCounter.increment
        count2 <- liftEffect BareCounter.currentCount
        count2 `assertEqual` 2

        -- `startPhase1` goes into a `handle_continue` that should double the current count
        -- and then add 1 to it
        liftEffect BareCounter.startPhase1Cast
        count3 <- liftEffect BareCounter.currentCount
        count3 `assertEqual` 5

        -- we can also send a message to start phase1 and go through the same flow
        counterPid <- liftEffect BareCounter.getPid
        BareCounterTypes.StartPhase1 # Process.send counterPid # liftEffect
        count4 <- liftEffect BareCounter.currentCount
        count4 `assertEqual` 11

        -- shut down the server and wait for `terminate` to be called
        -- `terminate` for the counter process sends back a message with
        -- whichever atom we've specified in `TestCounter.shutdown` so we can
        -- verify that we have indeed run `terminate`
        self <- liftEffect selfPid
        liftEffect $ BareCounter.shutdown self (Atom.atom "our_test_atom")
        result <- liftEffect receiveAtom
        result `assertEqual` Just (Atom.atom "our_test_atom")

foreign import receiveAtom :: Effect (Maybe Atom)
foreign import selfPid :: forall a. Effect (Process a)
