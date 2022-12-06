module App exposing (init, subscriptions, update, view)

import Html exposing (Html)
import Html.Attributes as Hats
import Model exposing (Model, draggingSelection)
import Msg exposing (Msg)


init : () -> ( Model, Cmd Msg )
init =
    always ( Model.init, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Msg.MouseDown coords -> (Model.startMouseDown coords model, Cmd.none)
        Msg.MouseUp coords -> (Model.endMouseDown coords model, Cmd.none)
        Msg.MouseMove coords -> (Model.mouseMove coords model, Cmd.none)
        _ ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    if draggingSelection model then
        Sub.batch 
            [ Msg.mouseUpSub
            , Msg.mouseMoveSub
            ]
    else
        Sub.batch
            [ Msg.mouseDownSub
            ]


view : Model -> Html Msg
view model =
    viewTanks False (Model.playerTanks model)
    ++ viewTanks True (Model.enemyTanks model)
    ++ viewSelectionBox model
    |> Html.main_ []


viewTanks : Bool -> List { coords : (Float, Float), selected : Bool, highlighted : Bool } -> List (Html Msg)
viewTanks flipped =
    List.concatMap (\{coords, selected, highlighted} -> viewTank flipped selected highlighted (Tuple.first coords) (Tuple.second coords))

viewTank : Bool -> Bool -> Bool -> Float -> Float -> List (Html Msg)
viewTank flipped selected highlighted x y =
    let
        translation = 
            [ "translate("
            , String.fromFloat x
            , "px, "
            , String.fromFloat y
            , "px)"
            ]
            |> String.join ""
        flip = 
            [ "scale("
            , if flipped then "-1, 1" else "1"
            , ")"
            ]
            |> String.join ""
    in
    [ Html.div
        [ Hats.classList
            [ ("tank", True)
            , ("selected", selected)
            , ("highlighted", highlighted)
            ]
        , Hats.style "transform" (translation ++ " " ++ flip)
        ]
        []
    ]


viewSelectionBox : Model -> List (Html Msg)
viewSelectionBox model =
    let
        (x, y) = Model.dragStartCoords model
        (ex, ey) = Model.dragEndCoords model
        (w, h) = (ex - x |> abs, ey - y |> abs)
        translation = 
            [ "translate("
            , String.fromFloat (min x ex)
            , "px, "
            , String.fromFloat (min y ey)
            , "px)"
            ]
            |> String.join ""
    in
    if draggingSelection model then
        [ Html.div
            [ Hats.id "selection-box" 
            , Hats.style "transform" translation
            , Hats.style "width" (String.fromFloat w ++ "px")
            , Hats.style "height" (String.fromFloat h ++ "px")
            ]
            []
        ]
    else
        []

