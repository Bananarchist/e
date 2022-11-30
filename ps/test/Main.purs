module Test.Main where

import Note (frequencyForNote)
import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class.Console (log)
import Test.Spec (pending, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  describe "Piano tests" do
     it "Produces good notes for good frequencies" do
      frequencyForNote "A" 4.0 `shouldEqual` 440.0

