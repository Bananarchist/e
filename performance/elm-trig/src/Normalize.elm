module Normalize exposing (normalize1, normalize2)

normalize1 : Float -> Float
normalize1 x =
    clamp -pi pi <|
        (x - 2 * pi * toFloat (round (x / (2 * pi))))

normalize2 : Float -> Float
normalize2 x =
    atan2 (sin x) (cos x)
