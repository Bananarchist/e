module Model exposing (Model, init, mouseCoords, mouseCoordinateType, MouseCoords, draggingSelection, playerTanks, enemyTanks, dragEndCoords, dragStartCoords, startMouseDown, endMouseDown, mouseMove, selectedEntities, selectionBoundingBox)

import Point2d
import Pixels
import Vector2d
import BoundingBox2d
import Direction2d
import Dict exposing (Dict)
import Set exposing (Set)

tankWidth = Pixels.float 64
tankHeight = Pixels.float 64

tankBoundingBox : ScreenCoords -> BoundingBox2d.BoundingBox2d Pixels.Pixels ScreenCoordinateSystem
tankBoundingBox coords = BoundingBox2d.from coords (Point2d.translateBy (Vector2d.withLength (Pixels.float 64) (Direction2d.degrees 135) ) coords)

selectionBoundingBox : Model -> (BoundingBox2d.BoundingBox2d Pixels.Pixels ScreenCoordinateSystem)
selectionBoundingBox ((Model controls _) as model) = 
    BoundingBox2d.from controls.dragStartCoords controls.dragEndCoords

selected : (BoundingBox2d.BoundingBox2d Pixels.Pixels ScreenCoordinateSystem) -> Model -> Bool
selected bb =
    selectionBoundingBox
    >> BoundingBox2d.intersects bb

highlighted : (BoundingBox2d.BoundingBox2d Pixels.Pixels ScreenCoordinateSystem) -> Model -> Bool
highlighted bb model =
    if draggingSelection model then
        selectionBoundingBox model
        |> BoundingBox2d.intersects bb
    else
        False


uncurry : (a -> b -> c) -> (a, b) -> c
uncurry fn (p1, p2) = fn p1 p2

type ScreenCoordinateSystem = ScreenCoordinateSystem
type WorldCoordinateSystem = WorldCoordinateSystem

type alias ScreenCoords = Point2d.Point2d Pixels.Pixels ScreenCoordinateSystem

type Model 
    = Model Controls Entities

type alias Controls =
    { dragging : Bool 
    , dragStartCoords : ScreenCoords
    , dragEndCoords : ScreenCoords
    }

type alias Entities = 
    { coords : Dict Int ScreenCoords
    , players : Set Int
    , selected : Set Int
    , highlighted : Set Int
    } 

type alias Entity =
    { controlledByPlayer : Bool
    , coords : ScreenCoords
    }

init : Model
init = Model initControls initEntities

initControls : Controls
initControls = 
    { dragging = False
    , dragStartCoords = Point2d.origin
    , dragEndCoords = Point2d.origin
    }

initEntities : Entities 
initEntities =
    { coords = Dict.fromList 
        [ (1, Point2d.pixels 30 50)
        , (2, Point2d.pixels 100 200)
        , (3, Point2d.pixels 100 400)
        , (4, Point2d.pixels 310 100)
        , (5, Point2d.pixels 300 200)
        , (6, Point2d.pixels 350 300)
        ]
    , players = Set.fromList [4, 5, 6]
    , selected = Set.empty
    , highlighted = Set.empty
    }

type alias MouseCoords = (MouseCoordinate, MouseCoordinate)

type alias MouseCoordinate = Float

mouseCoords : MouseCoordinate -> MouseCoordinate -> MouseCoords
mouseCoords = Tuple.pair

mouseCoordinateType : Float -> MouseCoordinate
mouseCoordinateType = identity

dragStartCoords : Model -> (Float, Float)
dragStartCoords (Model controls _) =
    controls.dragStartCoords |> Point2d.toTuple Pixels.toFloat

dragEndCoords : Model -> (Float, Float)
dragEndCoords (Model controls _) =
    controls.dragEndCoords |> Point2d.toTuple Pixels.toFloat

draggingSelection : Model -> Bool
draggingSelection (Model controls _) =
    controls.dragging 

startMouseDown : MouseCoords -> Model -> Model
startMouseDown mc (Model controls e) =
    Model { controls | dragStartCoords = (uncurry Point2d.pixels mc), dragEndCoords = (uncurry Point2d.pixels mc), dragging = True } e

endMouseDown : MouseCoords -> Model -> Model
endMouseDown mc (Model controls e) =
    Model { controls | dragEndCoords = (uncurry Point2d.pixels mc), dragging = False } e
    |> selectCapturedEntities
    |> clearHighlighted
    -- test for selected

mouseMove : MouseCoords -> Model -> Model
mouseMove mc (Model controls e) =
    Model { controls | dragEndCoords = uncurry Point2d.pixels mc } e
    |> highlightCapturedEntities

clearHighlighted : Model -> Model
clearHighlighted (Model c entities) =
    Model c { entities | highlighted = Set.empty }

additiveSelection : Model -> Bool
additiveSelection = always False

selectCapturedEntities : Model -> Model
selectCapturedEntities ((Model c entities) as model) = 
    let
        oldSelected = entities.selected
        newlySelected =
            entities.coords
            |> Dict.filter (\k _ -> Set.member k entities.players)
            |> Dict.toList
            |> List.map (Tuple.mapSecond (isSelected model))
            |> List.filter Tuple.second
            |> List.map Tuple.first
            |> Set.fromList
        finalSelection =
            if additiveSelection model then
                Set.union oldSelected newlySelected
            else
                newlySelected

    in
    Model c { entities | selected = finalSelection }

highlightCapturedEntities : Model -> Model
highlightCapturedEntities ((Model c entities) as model) = 
    let
        newHl =
            entities.coords
            |> Dict.filter (\k _ -> Set.member k entities.players)
            |> Dict.toList
            |> List.map (Tuple.mapSecond (isHighlighted model))
            |> List.filter Tuple.second
            |> List.map Tuple.first
            |> Set.fromList
    in
    Model c { entities | highlighted = newHl }

isSelected : Model -> ScreenCoords -> Bool
isSelected model = 
    tankBoundingBox
    >> (\bb -> selected bb model)

isHighlighted : Model -> ScreenCoords -> Bool
isHighlighted model = 
    tankBoundingBox
    >> (\bb -> highlighted bb model)

anySelected : Model -> Bool
anySelected ((Model _ entities) as model) = Set.isEmpty entities.selected |> not

selectedEntities : Model -> List Entity
selectedEntities ((Model _ entities) as model) = 
    entities.coords
    |> Dict.filter (\k _ -> Set.member k entities.selected)
    |> Dict.foldl (\k v -> (::) { coords = v, controlledByPlayer = Set.member k entities.players }) []

controlledByPlayer : Entity -> Bool
controlledByPlayer = .controlledByPlayer

playerTanks : Model -> List { coords : ( Float, Float ), selected : Bool, highlighted : Bool}
playerTanks (Model _ entities) =
    entities.coords
    |> Dict.filter (\k _ -> Set.member k entities.players)
    |> Dict.foldl (\k v -> (::) {coords = Point2d.toTuple Pixels.toFloat v, highlighted = Set.member k entities.highlighted, selected = Set.member k entities.selected }) []

enemyTanks : Model -> List { coords : ( Float, Float ), selected : Bool, highlighted : Bool}
enemyTanks (Model _ entities) =
    entities.coords
    |> Dict.filter (\k _ -> Set.member k entities.players |> not)
    |> Dict.foldl (\k v -> (::) {coords = Point2d.toTuple Pixels.toFloat v, highlighted = False, selected = Set.member k entities.selected }) []
