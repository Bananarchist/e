module MouseSelectionTests exposing (suite)


import Test
import Expect
import Fuzz
import Model
import BoundingBox2d
import Quantity
import Pixels

expectNotEmpty : List a -> Expect.Expectation
expectNotEmpty l =
  Expect.greaterThan 0 (List.length l)

expectTrue : Bool -> Expect.Expectation
expectTrue = Expect.equal True

suite : Test.Test
suite = 
  [ Test.fuzz2 Fuzz.niceFloat Fuzz.niceFloat "Can drag selection" <|
    \x y ->
      let
          (sx, sy) = (300, 300)
          ((minX, minY), (maxX, maxY)) =
            ((min x sx, min y sy), (max x sx, max y sy))
          mbbb = 
            Model.init
            |> Model.startMouseDown (sx, sy)
            |> Model.mouseMove (x, y)
            |> Model.selectionBoundingBox
      in
      BoundingBox2d.extrema mbbb
      |> Expect.all
          [ .minX >> Quantity.equalWithin (Pixels.float 0.1) (Pixels.float minX) >> expectTrue
          , .minY >> Quantity.equalWithin (Pixels.float 0.1) (Pixels.float minY) >> expectTrue
          , .maxX >> Quantity.equalWithin (Pixels.float 0.1) (Pixels.float maxX) >> expectTrue
          , .maxY >> Quantity.equalWithin (Pixels.float 0.1) (Pixels.float maxY) >> expectTrue
          ]

  , Test.test "Can select tank with click" <|
    \_ ->
      let
          model = Model.init
          (unitX, unitY) = Model.playerTanks model |> List.head |> Maybe.map (.coords) |> Maybe.withDefault (0,0)
      in
      Model.startMouseDown (unitX, unitY) model
      |> Model.endMouseDown (unitX, unitY)
      |> Model.selectedEntities
      |> List.length
      |> Expect.equal 1
  , Test.test "Can select multiple tanks with drag" <|
    \_ ->
      let
          model = Model.init
          (unitX, unitY) = Model.playerTanks model |> List.head |> Maybe.map (.coords) |> Maybe.withDefault (0,0)
      in
      Model.startMouseDown (0, 0) model
      |> Model.endMouseDown (600, 600)
      |> Model.selectedEntities
      |> List.length
      |> Expect.greaterThan 1
  ]
  |> Test.describe "Mouse Selection"
