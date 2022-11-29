module KeyboardTests exposing (suite)

import Expect
import Quantity
import Duration
import Test 
import Fuzz
import Model
import Main
import Msg
import Point3d exposing (Point3d)
import Length

mainUpdate : Msg.Msg -> Model.Model -> Model.Model
mainUpdate msg =
  Main.update msg >> Tuple.first
  
kbArrowFuzzer : Fuzz.Fuzzer Model.KB
kbArrowFuzzer =
  Fuzz.oneOf
    [ Fuzz.constant Model.ForwardKey
    , Fuzz.constant Model.ReverseKey
    , Fuzz.constant Model.StrafeLeftKey
    , Fuzz.constant Model.StrafeRightKey
    ]

kbFuzzer : Fuzz.Fuzzer Model.KB
kbFuzzer =
  Fuzz.oneOf
    [ Fuzz.constant Model.UntrackedKey
    , kbArrowFuzzer
    ]

suite : Test.Test
suite =
  [ Test.fuzz kbArrowFuzzer "Pressed arrow key saved" <|
    \k ->
      Model.init 1 1
      |> mainUpdate (Msg.KeyPressed k)
      |> Model.pressedKey
      |> Expect.equal (Just k)
  , Test.fuzz kbArrowFuzzer "Released arrow key poofs" <|
    \k ->
      Model.init 1 1
      |> mainUpdate (Msg.KeyPressed k)
      |> mainUpdate (Msg.KeyDepressed k)
      |> Model.pressedKey
      |> Expect.equal Nothing
  , Test.fuzz2 kbArrowFuzzer kbArrowFuzzer "Newly pressed arrow key replaces last" <|
    \k1 k2 ->
      Model.init 1 1
      |> mainUpdate (Msg.KeyPressed k1)
      |> mainUpdate (Msg.KeyPressed k2)
      |> Model.pressedKey
      |> Expect.equal (Just k2)
  , Test.fuzz kbArrowFuzzer "Newly pressed untracked key does not replace tracked arrow" <|
    \k ->
      Model.init 1 1
      |> mainUpdate (Msg.KeyPressed k)
      |> mainUpdate (Msg.KeyPressed Model.UntrackedKey)
      |> Model.pressedKey
      |> Expect.equal (Just k)
  , Test.fuzz kbArrowFuzzer "Releasing untracked key does not release tracked arrow key" <|
    \k ->
      Model.init 1 1
      |> mainUpdate (Msg.KeyPressed Model.UntrackedKey)
      |> mainUpdate (Msg.KeyPressed k)
      |> mainUpdate (Msg.KeyDepressed Model.UntrackedKey)
      |> Model.pressedKey
      |> Expect.equal (Just k)

  , Test.fuzz kbFuzzer "Frame advancement with pressed direction key results in changed position" <|
    \k ->
      let
        model = Model.init 1 1
        testPoint = Point3d.meters 1 1 1
        testPointRelativeToFrame = Point3d.relativeTo (Model.cameraFrame model) testPoint
        updatedModel = 
          model
          |> mainUpdate (Msg.KeyPressed k)
          |> mainUpdate (Msg.Frame (Duration.second))
      in
      testPoint
      |> Point3d.relativeTo (Model.cameraFrame updatedModel)
      |> Point3d.equalWithin (Length.centimeters 1) testPointRelativeToFrame
      |> Expect.equal (k == Model.UntrackedKey)
      |> Expect.onFail ("Failed equality test with 10cm tolerance for key = " ++ Debug.toString k)
  ]
  |> Test.describe "Keyboard tracking tests"
