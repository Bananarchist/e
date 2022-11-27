port module Main exposing (main)

import Browser
import Browser.Events
import Html
import Json.Decode
import Svg
import Svg.Attributes


port padButtonsDown : (Bool -> msg) -> Sub msg


port checkButtons : () -> Cmd msg


port initializeAudio : () -> Cmd msg


port startFart : () -> Cmd msg
port stopFart : () -> Cmd msg


port gamePadConnected : (Bool -> msg) -> Sub msg


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


flip : (a -> b -> c) -> b -> a -> c
flip fn p2 p1 =
    fn p1 p2


type alias System =
    { listening : Bool
    , mouseDown : Bool
    , keyDown : Bool
    , padBtnDown : Bool
    , messages : List ( String, Float )
    }


type alias Vector =
    { x : Float
    , y : Float
    , θ : Float
    }


type Model
    = Model System Vector


system : Model -> System
system (Model sys _) =
    sys


vector : Model -> Vector
vector (Model _ vec) =
    vec


setKeyDown : Bool -> Model -> Model
setKeyDown val (Model sys vec) =
    Model { sys | keyDown = val } vec


setMouseDown : Bool -> Model -> Model
setMouseDown val (Model sys vec) =
    Model { sys | mouseDown = val } vec


setPadDown : Bool -> Model -> Model
setPadDown val (Model sys vec) =
    Model { sys | padBtnDown = val } vec


setListening : Bool -> Model -> Model
setListening val (Model sys vec) =
    Model { sys | listening = val } vec


ifListening : (Model -> Model) -> Model -> Model
ifListening fn ((Model { listening } _) as model) =
    if listening then
        fn model

    else
        model


clearInput : Model -> Model
clearInput =
    setKeyDown False
        >> setMouseDown False
        >> setPadDown False


type Status
    = Moving
    | Rotating


status : Model -> Status
status (Model { mouseDown, keyDown, padBtnDown } _) =
    if mouseDown || keyDown || padBtnDown then
        Moving

    else
        Rotating


type Msg
    = KeyDown Bool
    | MouseDown Bool
    | PadBtnDown Bool
    | FrameΔ Float
    | GamePadConnected Bool
    | VisibilityChanged Browser.Events.Visibility


rotationFrequency : Float
rotationFrequency =
    pi / 2


movementSpeed : Float
movementSpeed =
    50


xTrigFn : Float -> Float
xTrigFn θ =
    if θ == pi || θ == 0 then
        0

    else if θ == pi / 2 then
        1

    else if θ == 3 * pi / 2 then
        -1

    else if θ < pi / 2 then
        sin θ

    else if θ < pi then
        sin θ

    else if θ < 3 * pi / 2 then
        sin θ

    else
        sin θ



-- perhaps all of these are negated BECAUSE we get
-- weirdness with positive y being down?
-- and are we able to use the same function BECAUSE
-- of something to do with complementary math via
-- the quadrants?????


yTrigFn : Float -> Float
yTrigFn θ =
    if θ == pi / 2 || θ == 3 * pi / 2 then
        0

    else if θ == pi then
        1

    else if θ == 0 then
        -1

    else if θ < pi / 2 then
        cos θ |> negate

    else if θ < pi then
        cos θ |> negate

    else if θ < 3 * pi / 2 then
        cos θ |> negate

    else
        cos θ |> negate


width : Float
width =
    30


height : Float
height =
    30


addTheta : Float -> Float -> Float
addTheta θ δ =
    θ
        + δ
        |> (\v ->
                if v > 2 * pi then
                    v - 2 * pi

                else if v < 0 then
                    v + 2 * pi

                else
                    v
           )


initialAngle : Float
initialAngle =
    pi + (10 * pi / 12)


initialSystem : System
initialSystem =
    { listening = True
    , keyDown = False
    , mouseDown = False
    , padBtnDown = False
    , messages = []
    }


initialVector : Vector
initialVector =
    { x = 100
    , y = 100
    , θ = initialAngle
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model initialSystem initialVector, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions (Model sys _) =
    if sys.listening then
        Sub.batch
            [ Browser.Events.onAnimationFrameDelta FrameΔ
            , Browser.Events.onKeyUp (Json.Decode.succeed (KeyDown False))
            , Browser.Events.onMouseUp (Json.Decode.succeed (MouseDown False))
            , Browser.Events.onKeyDown (Json.Decode.succeed (KeyDown True))
            , Browser.Events.onMouseDown (Json.Decode.succeed (MouseDown True))
            , Browser.Events.onVisibilityChange VisibilityChanged
            , padButtonsDown PadBtnDown
            , gamePadConnected GamePadConnected
            ]

    else
        Sub.batch
            [ Browser.Events.onVisibilityChange VisibilityChanged ]


sansCmd : Model -> ( Model, Cmd Msg )
sansCmd model =
    ( model, Cmd.none )


fartOnKeyDown : Bool -> Model -> ( Model, Cmd Msg )
fartOnKeyDown newKeyDown model =
    if newKeyDown then
        ( model, startFart () )

    else
        ( model, stopFart () )


addMessage : String -> Model -> Model
addMessage msg (Model sys vec) =
    Model { sys | messages = sys.messages ++ [ ( msg, 500 ) ] } vec


updateMessages : Float -> Model -> Model
updateMessages δ (Model sys vec) =
    Model
        { sys
            | messages =
                List.map (\( str, rem ) -> ( str, rem - δ )) sys.messages
                    |> List.filter (Tuple.second >> (<) 0)
        }
        vec


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ((Model sys vec) as model) =
    case msg of
        KeyDown state ->
            ifListening (setKeyDown state) model
                |> fartOnKeyDown state

        MouseDown state ->
            ifListening (setMouseDown state) model
                |> fartOnKeyDown state

        PadBtnDown state ->
            ifListening (setPadDown state) model
                |> fartOnKeyDown state

        VisibilityChanged Browser.Events.Visible ->
            setListening True model |> sansCmd

        VisibilityChanged Browser.Events.Hidden ->
            setListening False model
                |> clearInput
                |> sansCmd

        FrameΔ δ ->
            case status model of
                Moving ->
                    Model sys
                        { vec
                            | x = vec.x + (movementSpeed * δ / 1000) * xTrigFn vec.θ
                            , y = vec.y + (movementSpeed * δ / 1000) * yTrigFn vec.θ
                        }
                        |> updateMessages δ
                        |> flip Tuple.pair (checkButtons ())

                Rotating ->
                    Model sys
                        { vec
                            | θ = addTheta vec.θ (rotationFrequency * δ / 1000)
                        }
                        |> updateMessages δ
                        |> flip Tuple.pair (checkButtons ())

        GamePadConnected state ->
            if state then
                addMessage "Game pad connected" model
                    |> sansCmd

            else
                addMessage "Game pad disconnected" model
                    |> sansCmd


view : Model -> Html.Html Msg
view model =
    viewMessages model
        ++ viewField model
        |> Html.main_ []


viewField : Model -> List (Html.Html Msg)
viewField =
    viewSquare
        >> (++) [ Svg.defs [] [ gradient ] ]
        >> Svg.svg [ Svg.Attributes.viewBox "0 0 1000 1000" ]
        >> List.singleton


viewMessages : Model -> List (Html.Html Msg)
viewMessages (Model sys _) =
    List.map (Tuple.first >> Html.text >> List.singleton >> Html.div []) sys.messages


viewSquare : Model -> List (Html.Html Msg)
viewSquare model =
    [ Svg.Attributes.stroke "black"
    , Svg.Attributes.fill "url('#fireButt')"
    , Svg.Attributes.width (String.fromFloat width)
    , Svg.Attributes.height (String.fromFloat height)
    ]
        ++ coords model
        ++ rotation model
        |> flip Svg.rect []
        |> List.singleton


coords : Model -> List (Svg.Attribute Msg)
coords (Model _ vec) =
    [ Svg.Attributes.x (String.fromFloat vec.x), Svg.Attributes.y (String.fromFloat vec.y) ]


rotation : Model -> List (Svg.Attribute Msg)
rotation (Model _ vec) =
    let
        rotate x y θ =
            [ "rotate("
            , θ * 180 / pi |> String.fromFloat
            , String.fromFloat (x + (width / 2))
            , String.fromFloat (y + (height / 2))
            , ")"
            ]
                |> String.join " "
    in
    [ Svg.Attributes.transform (rotate vec.x vec.y vec.θ) ]


gradient : Svg.Svg Msg
gradient =
    Svg.linearGradient
        [ Svg.Attributes.id "fireButt"
        , Svg.Attributes.gradientTransform "rotate(-270)"
        ]
        [ Svg.stop
            [ Svg.Attributes.offset "65%", Svg.Attributes.stopColor "white" ]
            []
        , Svg.stop
            [ Svg.Attributes.offset "100%", Svg.Attributes.stopColor "red" ]
            []
        ]
