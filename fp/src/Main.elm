module Main exposing (main)

import Browser
import Html exposing (Html)
import Model exposing (Model)
import Msg exposing (Msg)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


init : () -> ( Model, Cmd Msg )
init =
    always ( Model.init, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update _ model =
    ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions =
    always Sub.none


view : Model -> Html Msg
view =
    always <|
        Html.main_
            []
            [ Html.text "Content goes here" ]
