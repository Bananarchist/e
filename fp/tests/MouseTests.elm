module MouseTests exposing (suite)

import Expect
import Quantity
import Duration
import Test 
import Fuzz
import Model
import Main
import Msg
import Point3d exposing (Point3d)
import Pixels
import Length

mainUpdate : Msg.Msg -> Model.Model -> Model.Model
mainUpdate msg =
  Main.update msg >> Tuple.first

bilateralFloat : Float -> Fuzz.Fuzzer Float
bilateralFloat m =
  Fuzz.floatRange (negate m) m

mouseMovement = 
  Fuzz.map2 
    Msg.MouseMoved 
    (Fuzz.floatRange -50 50 |> Fuzz.map Pixels.float) 
    (Fuzz.floatRange -50 50 |> Fuzz.map Pixels.float)


suite : Test.Test
suite =
  [ Test.fuzz2 (bilateralFloat 50 |> Fuzz.map Pixels.float) (bilateralFloat 50 |> Fuzz.map Pixels.float) "camera focal point changes with mouse movement" <|
    \mx my -> 
      let
        model = Model.init 1000 1000
        testPoint = Point3d.meters 1 1 1
        testPointProjected = testPoint |> Point3d.relativeTo (Model.cameraFrame model)
        updated = 
          model
            |> mainUpdate (Msg.MouseMoved mx my)
      in
      testPoint
      |> Point3d.relativeTo (Model.cameraFrame updated)
      |> Point3d.equalWithin (Length.millimeters 0.1) testPointProjected
      |> Expect.equal (Quantity.equalWithin (Pixels.float 0.1) mx my && Quantity.equalWithin (Pixels.float 0.1) mx Quantity.zero)
      |> Expect.onFail ("Og frame: " ++ (Debug.toString (testPointProjected)) ++ " and updated: " ++ (Debug.toString (Point3d.relativeTo (Model.cameraFrame updated) testPoint)))
  , Test.todo "mouse camera movement capped looking directly up"
  , Test.todo "mouse camera movement capped looking directly down"
  ]
  |> Test.describe "Keyboard tracking tests"
