module AerialDrawing exposing (main)

import Angle
import Browser
import Browser.Dom
import Browser.Events
import Camera3d
import Color
import Direction3d
import Html exposing (Html)
import Html.Attributes exposing (style)
import Json.Decode as Decode exposing (Decoder)
import Length
import Pixels
import Point3d
import Quantity exposing (Quantity)
import Scene3d
import Scene3d.Material as Material
import Task
import Vector3d exposing (Vector3d)
import Viewpoint3d


main : Program () Model Msg
main =
    Browser.element { init = init, update = update, subscriptions = subscriptions, view = view }



-- INIT


init : () -> ( Model, Cmd Msg )
init () =
    ( { width = Quantity.zero
      , height = Quantity.zero
      , eyePoint = { x = 0.0, y = 0.0, z = 0.0 }
      , focalVector = { x = 1.0, y = 0.0, z = 0.0 }
      , azimuth = Angle.degrees -90
      , elevation = Angle.degrees 30
      }
    , Task.perform
        (\{ viewport } ->
            ResizeWindow
                (Pixels.int (round viewport.width))
                (Pixels.int (round viewport.height))
        )
        Browser.Dom.getViewport
    )



-- MODEL


type alias Model =
    { width : Quantity Int Pixels.Pixels
    , height : Quantity Int Pixels.Pixels
    , eyePoint : Point3dModel
    , focalVector : Point3dModel
    , azimuth : Angle.Angle
    , elevation : Angle.Angle
    }


type alias Point3dModel =
    { x : Float
    , y : Float
    , z : Float
    }



-- MSG


type Msg
    = ResizeWindow (Quantity Int Pixels.Pixels) (Quantity Int Pixels.Pixels)
    | MouseMove (Quantity Float Pixels.Pixels) (Quantity Float Pixels.Pixels)



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ResizeWindow width height ->
            ( { model | width = width, height = height }, Cmd.none )

        MouseMove dx dy ->
            let
                dxFloat =
                    Pixels.toFloat dx

                dyFloat =
                    Pixels.toFloat dy

                newFocalVector =
                    { x =
                        if model.focalVector.y > 0.0 then
                            model.focalVector.x + dxFloat / 200

                        else
                            model.focalVector.x - dxFloat / 200
                    , y =
                        if model.focalVector.x > 0.0 then
                            model.focalVector.y - dxFloat / 200

                        else
                            model.focalVector.y + dxFloat / 200
                    , z = model.focalVector.z - dyFloat / 200

                    -- z = model.focalVector.z
                    }
            in
            ( { model | focalVector = newFocalVector }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Browser.Events.onResize (\width height -> ResizeWindow (Pixels.int width) (Pixels.int height))
        , Browser.Events.onMouseMove mouseMoveDecoder
        ]



-- VIEW


view : Model -> Html Msg
view model =
    let
        square =
            Scene3d.quad (Material.color Color.blue)
                (Point3d.meters -100 -100 -1)
                (Point3d.meters 100 -100 -1)
                (Point3d.meters 100 100 -1)
                (Point3d.meters -100 100 -1)

        square2 =
            Scene3d.quad (Material.color Color.red)
                (Point3d.meters 100 -100 -100)
                (Point3d.meters 100 -100 100)
                (Point3d.meters 100 100 100)
                (Point3d.meters 100 100 -100)

        square3 =
            Scene3d.quad (Material.color Color.green)
                (Point3d.meters -100 -100 -100)
                (Point3d.meters -100 -100 100)
                (Point3d.meters -100 100 100)
                (Point3d.meters -100 100 -100)

        square4 =
            Scene3d.quad (Material.color Color.yellow)
                (Point3d.meters -100 -100 -100)
                (Point3d.meters -100 -100 100)
                (Point3d.meters 100 -100 100)
                (Point3d.meters 100 -100 -100)

        camera =
            Camera3d.perspective
                { viewpoint =
                    Viewpoint3d.lookAt
                        { eyePoint = Point3d.meters model.eyePoint.x model.eyePoint.y model.eyePoint.z
                        , focalPoint = Point3d.meters (model.focalVector.x + model.eyePoint.x) (model.focalVector.y + model.eyePoint.y) (model.focalVector.z + model.eyePoint.z)
                        , upDirection = Direction3d.positiveZ
                        }
                , verticalFieldOfView = Angle.degrees 30
                }
    in
    Scene3d.unlit
        { entities = [ square, square2, square3, square4 ]
        , camera = camera
        , clipDepth = Length.meters 1
        , background = Scene3d.transparentBackground
        , dimensions = ( model.width, model.height )
        }



-- FUNCTION


mouseMoveDecoder : Decoder Msg
mouseMoveDecoder =
    Decode.map2 MouseMove
        (Decode.field "movementX" (Decode.map Pixels.float Decode.float))
        (Decode.field "movementY" (Decode.map Pixels.float Decode.float))
