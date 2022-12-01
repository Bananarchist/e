module Test.Main (main, fuzz) where

import Prelude

import Data.Array (length, (!!))
import Data.Maybe (fromMaybe)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Exception (catchException, message)
import Effect.Random (randomInt)
import Note (frequencyForNote)
import Test.Spec (SpecT, before, describe, evaluateExample, it, pending, parallel)
import Test.Spec.Assertions (shouldEqual, fail)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)
import Test.Spec.Tree (ActionWith)


uniformFuzz :: forall a. a -> Array a -> Effect a
uniformFuzz default values = do
  let totalValues = length values
  randomIdx <- randomInt 0 $ (totalValues - 1)
  pure $ fromMaybe default $ values !! randomIdx


fuzzedNote :: Effect String
fuzzedNote = do
  uniformFuzz "Ab" [ "A", "Bb", "B", "C", "Db", "D", "Eb", "E", "F", "Gb", "G" ]

-- can't get type string for this correct but the compiler understands...
fuzz fuzzer title test =
  before (liftEffect fuzzer) do
    it title \val ->
      liftEffect $
        catchException 
          (\e -> do
            log ("Given: " <> show val)
            fail $ message e
          )
          (test val)
          


main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  describe "Piano tests" do
     fuzz fuzzedNote "Produces good frequencies for notes"
       (\note -> do frequencyForNote note 4.0 `shouldEqual` 440.0)
