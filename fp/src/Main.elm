module Main exposing (main, init, update, view)

import Angle
import Browser
import Camera3d
import Color
import Direction3d
import Helpers exposing (duple, tupleMap)
import Html exposing (Html)
import Length
import Model exposing (Model, WorldCoordinateSystem)
import Msg exposing (Msg)
import Pixels
import Point3d
import Sphere3d
import Scene3d
import Viewpoint3d
import Scene3d.Material as Material


main : Program { width : Int, height : Int } Model Msg
main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


init : { width : Int, height : Int } -> ( Model, Cmd Msg )
init { width, height } =
    ( Model.init width height, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Msg.KeyDepressed k ->
            ( Model.releaseKey k model, Cmd.none )
        Msg.KeyPressed k ->
            ( Model.pressKey k model, Cmd.none )
        Msg.Frame t ->
            ( Model.newFrame t model, Cmd.none )
        Msg.MouseMoved δx δy ->
            ( Model.moveMouse δx δy model, Cmd.none)


subscriptions : Model -> Sub Msg
subscriptions =
    always <|
        Sub.batch
            [ Msg.keyDownSub
            , Msg.keyUpSub
            , Msg.animationFrameSub
            ]


view : Model -> Html Msg
view model =
    Html.main_
        []
        [ viewScene model ]


viewScene : Model -> Html Msg
viewScene model =
    Scene3d.unlit
        { dimensions = duple model |> Tuple.mapBoth (Model.width >> Pixels.pixels) (Model.height >> Pixels.pixels)
        , camera = Camera3d.perspective 
            { viewpoint = viewPoint model
            , verticalFieldOfView = Angle.degrees 50
            }
        , clipDepth = Length.meters 1
        , background = Scene3d.backgroundColor Color.white
        , entities = Model.entities model
        }


viewPoint : Model -> Viewpoint3d.Viewpoint3d Length.Meters Model.CameraCoordinateSystem
viewPoint model =
        Viewpoint3d.lookAt
            { eyePoint = Model.cameraPoint3d model
            , focalPoint = Point3d.origin
            , upDirection = Direction3d.positiveZ
            }

