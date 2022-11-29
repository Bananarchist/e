module Model exposing (Model, init, width, height, cameraPoint3d, WorldCoordinateSystem, KB(..), pressKey, releaseKey, pressedKey, newFrame, cameraFocalPoint3d, moveMouse, entities, CameraCoordinateSystem, cameraFrame)

import Angle
import Point3d
import Duration exposing (Duration)
import Vector3d exposing (Vector3d)
import Length exposing (Meters)
import Speed exposing (Speed)
import Quantity exposing (Quantity)
import Direction3d
import Pixels exposing (Pixels)
import Sphere3d
import Axis3d
import Scene3d.Material as Material
import Color
import Scene3d
import Frame3d exposing (Frame3d)

type CameraCoordinateSystem
    = CameraCoordinateSystem

type WorldCoordinateSystem
    = WorldCoordinateSystem

type KB
  = ForwardKey
  | ReverseKey
  | StrafeLeftKey
  | StrafeRightKey
  | UntrackedKey

{-
  Probably not going to work to keep focal point
  Probably want a vector for direction (helps with movement too)
  Scale the vector by 50 and apply it as a translation on the position to get the focal point

-}

type alias ViewPoint =
    { eyePoint : Point3d.Point3d Meters WorldCoordinateSystem
    , focalPoint : Point3d.Point3d Meters WorldCoordinateSystem
    , focalVector : Vector3d Meters WorldCoordinateSystem
    , frame : Frame3d Meters WorldCoordinateSystem { defines : CameraCoordinateSystem }
    }

type Model 
    = Model Int Int ViewPoint (Maybe KB) (Duration.Duration, Duration.Duration)

init : Int -> Int -> Model
init w h = Model w h (ViewPoint (Point3d.meters 10 0 5) (Point3d.meters 0 0 0) (Vector3d.meters 1 0 0.5) (Frame3d.atOrigin)) Nothing (Duration.seconds 0, Duration.seconds 0)

width : Model -> Int
width (Model w _ _ _ _) = w

height : Model -> Int
height (Model _ h _ _ _) = h

cameraPoint3d : Model -> Point3d.Point3d Meters CameraCoordinateSystem
cameraPoint3d (Model _ _ {eyePoint} _ _) = Point3d.meters 0 -10 0 --eyePoint

cameraFocalPoint3d : Model -> Point3d.Point3d Meters WorldCoordinateSystem
cameraFocalPoint3d (Model _ _ {focalPoint} _ _) = focalPoint

cameraFrame : Model -> Frame3d Meters WorldCoordinateSystem { defines : CameraCoordinateSystem }
cameraFrame (Model _ _ {frame} _ _) = frame

pressKey : KB -> Model -> Model
pressKey k = 
    case k of
        UntrackedKey -> identity
        _ -> setKeyValue k True

releaseKey : KB -> Model -> Model
releaseKey k =
    case k of
        UntrackedKey -> identity
        _ -> setKeyValue k False

setKeyValue : KB -> Bool -> Model -> Model
setKeyValue k p (Model w h c kp d) =
    if not p && ((Just k) == kp) then
        Model w h c Nothing d
    else if p && (k == UntrackedKey) then
        Model w h c kp d
    else 
        Model w h c (Just k) d

pressedKey : Model -> Maybe KB
pressedKey (Model _ _ _ kp _) = kp

moveMouse : Quantity Float Pixels -> Quantity Float Pixels -> Model -> Model
moveMouse δx δy ((Model w h c kp d) as model) =
  let
      halfW = toFloat w / 2
      halfH = toFloat h / 2
      percentW = δx |> Quantity.divideBy halfW
      percentH = δy |> Quantity.divideBy halfH
      --| Rotates about z-axis
      angleX = Angle.degrees (percentW |> Quantity.multiplyBy 90 |> Pixels.toFloat)
      --| Rotates about... an axis
      angleY = Angle.degrees (percentH |> Quantity.multiplyBy 90 |> Pixels.toFloat)
      --| Get axis about which to rotate Y
      maybeRotate =
        c.focalVector
          |> Vector3d.rotateAround Axis3d.z (Angle.degrees 90)
          |> Vector3d.toTuple Length.inMeters
          |> (\(x,y,_) -> (x,y,0))
          |> Vector3d.fromTuple Length.meters
          |> Vector3d.direction
          |> Maybe.map (Axis3d.through Point3d.origin)
          |> Maybe.map (\a -> Vector3d.rotateAround a angleY)
          |> Maybe.withDefault identity
      
  in
  mapFocalVector (Vector3d.rotateAround Axis3d.z angleX >> maybeRotate) model
  |> mapFrame (Frame3d.rotateAround Axis3d.z angleX)
  |> mapFrame (Frame3d.rotateAround Axis3d.x angleY)

mapEyePoint : (Point3d.Point3d Meters WorldCoordinateSystem -> Point3d.Point3d Meters WorldCoordinateSystem) -> Model -> Model
mapEyePoint fn (Model w h c kp d) =
  Model w h { c | eyePoint = fn c.eyePoint } kp d

mapFocalPoint : (Point3d.Point3d Meters WorldCoordinateSystem -> Point3d.Point3d Meters WorldCoordinateSystem) -> Model -> Model
mapFocalPoint fn (Model w h c kp d) =
  Model w h { c | focalPoint = fn c.focalPoint } kp d

mapFocalVector : (Vector3d Meters WorldCoordinateSystem -> Vector3d Meters WorldCoordinateSystem) -> Model -> Model
mapFocalVector fn (Model w h c kp d) =
  Model w h { c | focalVector = fn c.focalVector } kp d

mapFrame : 
  (Frame3d Meters WorldCoordinateSystem {defines : CameraCoordinateSystem } -> Frame3d Meters WorldCoordinateSystem {defines : CameraCoordinateSystem }) 
  -> Model 
  -> Model
mapFrame fn (Model w h c kp d) =
  Model w h { c | frame = fn c.frame } kp d


translateFocalPointByFocalVector : Model -> Model
translateFocalPointByFocalVector (Model w h c kp d) =
  Model w h { c | focalPoint = Point3d.translateBy c.focalVector c.focalPoint } kp d

newFrame : Duration -> Model -> Model
newFrame t ((Model w h c kp (d1, d2)) as model) =
  let 
      newD = (d2, t)
      δt = t |> Quantity.minus d2

      frameTranslation = 
        \strafe reverse ->
          if strafe then
            if reverse then
              Frame3d.translateBy (Vector3d.xyz (Quantity.at strafeSpeed δt |> Quantity.negate) Quantity.zero Quantity.zero)
            else
              Frame3d.translateBy (Vector3d.xyz (Quantity.at strafeSpeed δt) Quantity.zero Quantity.zero)
          else
            if reverse then
              Frame3d.translateBy (Vector3d.xyz Quantity.zero (Quantity.at advanceSpeed δt |> Quantity.negate) Quantity.zero)
            else
              Frame3d.translateBy (Vector3d.xyz Quantity.zero (Quantity.at advanceSpeed δt) Quantity.zero)
            
      modelWithNewD =
          Model w h c kp newD
      -- we need z vector to be zero c'\`)
      -- and we need to rotate by 90° for strafe
  in
  case pressedKey model of
    Just k ->
      case k of
          ForwardKey ->
            modelWithNewD
            |> mapFrame (frameTranslation False False)
          ReverseKey ->
            modelWithNewD
            |> mapFrame (frameTranslation False True)
          StrafeRightKey ->
            modelWithNewD
            |> mapFrame (frameTranslation True False)
          StrafeLeftKey ->
            modelWithNewD
            |> mapFrame (frameTranslation True True)
          _ -> 
            modelWithNewD
    Nothing ->
      modelWithNewD

advanceSpeed : Speed
advanceSpeed = Speed.metersPerSecond 1

strafeSpeed : Speed
strafeSpeed = Speed.metersPerSecond 0.5


xTranslationVector : Duration -> Vector3d Meters WorldCoordinateSystem
xTranslationVector t =
  Vector3d.withLength (Quantity.at strafeSpeed t) Direction3d.x

yTranslationVector : Duration -> Vector3d Meters WorldCoordinateSystem
yTranslationVector t =
  Vector3d.withLength (Quantity.at advanceSpeed t) Direction3d.y

entities : Model -> List (Scene3d.Entity CameraCoordinateSystem)
entities model =
  List.map2 
    (\s c -> Scene3d.sphereWithShadow 
      (Material.constant c |> Material.texturedMatte) 
      (Sphere3d.relativeTo (cameraFrame model) s)
    )
    [ (Sphere3d.atPoint
            (Point3d.meters -10 -2 2)
            (Length.meters 2)
        )
    , (Sphere3d.atPoint
            (Point3d.meters -6 2 2)
            (Length.meters 2)
        )
    , (Sphere3d.atPoint
            (Point3d.meters -26 9 2)
            (Length.meters 2)
        )
    , (Sphere3d.atPoint
            (Point3d.meters -3 -7 2)
            (Length.meters 2)
        )
    , (Sphere3d.atPoint
            (Point3d.meters -9 -22 1)
            (Length.meters 2)
        )
    ]
    [ Color.lightBlue, Color.lightGreen, Color.purple, Color.red, Color.yellow ]

