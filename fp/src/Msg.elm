module Msg exposing (Msg(..), keyDownSub, keyUpSub, animationFrameSub, mouseSub)

import Model exposing (KB(..))
import Browser.Events
import Json.Decode as Decode
import Duration
import Pixels
import Time
import Quantity exposing (Quantity)

type Msg 
  = KeyPressed KB
  | KeyDepressed KB
  | Frame Duration.Duration
  | MouseMoved (Quantity Float Pixels.Pixels) (Quantity Float Pixels.Pixels)

{- Subscriptions -}

animationFrameSub : Sub Msg
animationFrameSub = Browser.Events.onAnimationFrame (Time.posixToMillis >> toFloat >> Duration.milliseconds >> Frame)

{- Keyboard -}

keyDownSub : Sub Msg
keyDownSub = Browser.Events.onKeyDown (Decode.map KeyPressed keyDecoder)

keyUpSub : Sub Msg
keyUpSub = Browser.Events.onKeyUp (Decode.map KeyDepressed keyDecoder)

keyDecoder : Decode.Decoder KB
keyDecoder =
  Decode.map 
    (\str ->
      case str of
        "ArrowDown" -> ReverseKey
        "ArrowLeft" -> StrafeLeftKey
        "ArrowRight" -> StrafeRightKey
        "ArrowUp" -> ForwardKey
        _ -> UntrackedKey
    )
    (Decode.field "key" Decode.string)

{- Mouse -}
mouseSub : Sub Msg
mouseSub = Browser.Events.onMouseMove decodeMouseMovement

decodeMouseMovement : Decode.Decoder Msg
decodeMouseMovement =
  Decode.map2 MouseMoved
    (Decode.field "movementX" (Decode.map Pixels.float Decode.float))
    (Decode.field "movementY" (Decode.map Pixels.float Decode.float))
