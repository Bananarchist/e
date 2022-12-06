module Msg exposing (Msg(..), mouseClickSub, mouseDownSub, mouseUpSub, mouseMoveSub)

import Model
import Browser.Events exposing (onClick, onMouseDown, onMouseUp, onMouseMove)
import Json.Decode as Decode

type Msg
    = MouseDown Model.MouseCoords
    | MouseUp Model.MouseCoords
    | MouseClick Model.MouseCoords
    | MouseMove Model.MouseCoords

{- Mouse Evts -}
mouseMovementDecoder : Decode.Decoder Model.MouseCoords
mouseMovementDecoder =
    Decode.map2 Model.mouseCoords
        (Decode.field "movementX" (Decode.map Model.mouseCoordinateType Decode.float))
        (Decode.field "movementY" (Decode.map Model.mouseCoordinateType Decode.float)) 

mouseLocationDecoder : Decode.Decoder Model.MouseCoords
mouseLocationDecoder =
    Decode.map2 Model.mouseCoords
        (Decode.field "clientX" (Decode.map Model.mouseCoordinateType Decode.float))
        (Decode.field "clientY" (Decode.map Model.mouseCoordinateType Decode.float)) 

mouseDownSub = onMouseDown (Decode.map MouseDown mouseLocationDecoder)
mouseUpSub = onMouseUp (Decode.map MouseUp mouseLocationDecoder)
mouseClickSub = onClick (Decode.map MouseClick mouseLocationDecoder)
mouseMoveSub = onMouseMove (Decode.map MouseMove mouseLocationDecoder)

{- GFX Evts -}

