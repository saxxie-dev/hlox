module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Class.Console (log)
import Effect.Aff (launchAff_)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)
import Test.LoxScanner

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  loxScannerTests