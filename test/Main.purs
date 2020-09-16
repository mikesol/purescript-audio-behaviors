module Test.Main where

import Prelude
import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Basic (basicTestSuite)
import Test.LinProg1 (linprogTestSuite)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

main ∷ Effect Unit
main =
  launchAff_
    $ runSpec [ consoleReporter ] do
        basicTestSuite
        linprogTestSuite
