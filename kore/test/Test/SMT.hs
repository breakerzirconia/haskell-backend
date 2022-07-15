module Test.SMT (
    testPropertyWithSolver,
    testPropertyWithoutSolver,
    testCaseWithoutSMT,
    assertEqual',
    runSMT,
    runSMTWithConfig,
    runNoSMT,
) where

import qualified Control.Monad.Morph as Morph
import Hedgehog
import Log ( runLoggerT )
import Prelude.Kore
import qualified SMT
import SMT ( MSMT, SMT )
import Test.Tasty
import Test.Tasty.Hedgehog
import Test.Tasty.HUnit

testPropertyWithSolver ::
    HasCallStack =>
    String ->
    PropertyT MSMT () ->
    TestTree
testPropertyWithSolver str =
    testProperty str . Hedgehog.property . Morph.hoist (runSMT (pure ()))

testPropertyWithoutSolver ::
    HasCallStack =>
    String ->
    PropertyT MSMT () ->
    TestTree
testPropertyWithoutSolver str =
    testProperty str . Hedgehog.property . Morph.hoist runNoSMT

testCaseWithoutSMT :: String -> MSMT () -> TestTree
testCaseWithoutSMT str = testCase str . runNoSMT

assertEqual' ::
    MonadIO m =>
    (Eq a, Show a) =>
    HasCallStack =>
    -- | Remark
    String ->
    -- | Expected value
    a ->
    -- | Actual value
    a ->
    m ()
assertEqual' str expect = liftIO . assertEqual str expect

runSMT :: SMT () -> MSMT a -> IO a
runSMT = runSMTWithConfig SMT.defaultConfig

runSMTWithConfig :: SMT.Config -> SMT () -> MSMT a -> IO a
runSMTWithConfig config userInit = flip runLoggerT mempty . SMT.runWithSolver config userInit

runNoSMT :: MSMT a -> IO a
runNoSMT = flip runLoggerT mempty . SMT.runWithoutSolver
