port module AirArt exposing (main)

import Angle
import Browser
import Browser.Dom
import Browser.Events
import Camera3d
import Color exposing (Color)
import Direction3d
import Html exposing (Html, div, pre)
import Html.Attributes exposing (id, max, min, style, type_, value)
import Html.Events exposing (onInput, onMouseDown, onMouseUp)
import Illuminance
import Json.Decode as Decode exposing (Decoder)
import Length
import LuminousFlux exposing (LuminousFlux)
import Pixels
import Point3d
import Quantity exposing (Quantity)
import Scene3d
import Scene3d.Light as Light exposing (Chromaticity, Light)
import Scene3d.Material as Material
import SolidAngle
import Sphere3d
import Task
import Temperature
import Vector3d exposing (Vector3d)
import Viewpoint3d


port requestPointerLock : () -> Cmd msg


port exitPointerLock : () -> Cmd msg



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- INIT


init : () -> ( Model, Cmd Msg )
init () =
    ( { width = Quantity.zero
      , height = Quantity.zero
      , eyePoint = { x = 0.0, y = 0.0, z = 50.0 }
      , focalVector = { x = 1.0, y = 0.0, z = 0.0 }
      , keyStatus =
            { up = False
            , down = False
            , left = False
            , right = False
            , space = False
            , shift = False
            }
      , isClick = False
      , points = []
      , numPoints = 0
      , isOptionOpen = False
      , isViewMoveEnable = False
      , option =
            { moveSpeed = 3.0
            , viewMoveSpeed = 5.0
            , penSize = 50.0
            , penColor = "#000000"
            , penDistance = 20.0
            }
      }
    , Task.perform
        (\{ viewport } ->
            ResizeWindow
                (Pixels.int <| round viewport.width)
                (Pixels.int <| round viewport.height)
        )
        Browser.Dom.getViewport
    )



-- MODEL


type alias Model =
    { width : Quantity Int Pixels.Pixels
    , height : Quantity Int Pixels.Pixels
    , eyePoint : Point3dModel
    , focalVector : Point3dModel
    , keyStatus : KeyStatusModel
    , isClick : Bool
    , points : List PointModel
    , numPoints : Int
    , isOptionOpen : Bool
    , isViewMoveEnable : Bool
    , option : OptionModel
    }


type alias Point3dModel =
    { x : Float
    , y : Float
    , z : Float
    }


type alias KeyStatusModel =
    { up : Bool
    , down : Bool
    , left : Bool
    , right : Bool
    , space : Bool
    , shift : Bool
    }


type alias PointModel =
    { coord : Point3dModel
    , color : String
    , size : Float
    }


type alias OptionModel =
    { moveSpeed : Float
    , viewMoveSpeed : Float
    , penSize : Float
    , penColor : String
    , penDistance : Float
    }



-- MSG


type Msg
    = ResizeWindow (Quantity Int Pixels.Pixels) (Quantity Int Pixels.Pixels)
    | RequestPointerLock
    | KeyChanged Bool String
    | DisableIsClick
    | TimeDelta Float
    | ViewMove (Quantity Float Pixels.Pixels) (Quantity Float Pixels.Pixels)
    | DrawPoints (Quantity Float Pixels.Pixels) (Quantity Float Pixels.Pixels)
    | OpenOptionModal
    | CloseOptionModal
    | ChangeMoveSpeed Float
    | ChangeViewMoveSpeed Float
    | ChangePenSize Float
    | ChangePenColor String
    | ChangePenDistance Float



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ResizeWindow width height ->
            ( { model | width = width, height = height }, Cmd.none )

        RequestPointerLock ->
            if model.isOptionOpen == False then
                ( { model | isClick = True, isOptionOpen = False, isViewMoveEnable = True }, requestPointerLock () )

            else
                ( model, Cmd.none )

        OpenOptionModal ->
            ( { model | isOptionOpen = True }, Cmd.none )

        CloseOptionModal ->
            ( { model | isOptionOpen = False }, requestPointerLock () )

        ChangeMoveSpeed value ->
            ( { model | option = updateOption model.option "moveSpeed" <| String.fromFloat value }, Cmd.none )

        ChangeViewMoveSpeed value ->
            ( { model | option = updateOption model.option "viewMoveSpeed" <| String.fromFloat value }, Cmd.none )

        ChangePenSize value ->
            ( { model | option = updateOption model.option "penSize" <| String.fromFloat value }, Cmd.none )

        ChangePenColor color ->
            ( { model | option = updateOption model.option "penColor" color }, Cmd.none )

        ChangePenDistance value ->
            ( { model | option = updateOption model.option "penDistance" <| String.fromFloat value }, Cmd.none )

        TimeDelta dt ->
            let
                toUpVector =
                    { x = 0
                    , y = 0
                    , z = 1
                    }

                toDownVector =
                    { x = 0
                    , y = 0
                    , z = -1
                    }

                toRightVector =
                    { x = model.focalVector.x * cos (degrees 90) - model.focalVector.y * sin (degrees 90)
                    , y = model.focalVector.x * sin (degrees 90) + model.focalVector.y * cos (degrees 90)
                    , z = 0
                    }

                toLeftVector =
                    { x = model.focalVector.x * cos (degrees -90) - model.focalVector.y * sin (degrees -90)
                    , y = model.focalVector.x * sin (degrees -90) + model.focalVector.y * cos (degrees -90)
                    , z = 0
                    }

                position =
                    if model.keyStatus.up == True then
                        addPoints model.eyePoint <|
                            multiplePoints model.focalVector <|
                                model.option.moveSpeed
                                    * 0.1

                    else if model.keyStatus.down == True then
                        minusPoints model.eyePoint <|
                            multiplePoints model.focalVector <|
                                model.option.moveSpeed
                                    * 0.1

                    else if model.keyStatus.left == True then
                        addPoints model.eyePoint <|
                            multiplePoints toRightVector <|
                                model.option.moveSpeed
                                    * 0.1

                    else if model.keyStatus.right == True then
                        addPoints model.eyePoint <|
                            multiplePoints toLeftVector <|
                                model.option.moveSpeed
                                    * 0.1

                    else if model.keyStatus.space == True then
                        addPoints model.eyePoint <|
                            multiplePoints toUpVector <|
                                model.option.moveSpeed
                                    * 0.1

                    else if model.keyStatus.shift == True then
                        addPoints model.eyePoint <|
                            multiplePoints toDownVector <|
                                model.option.moveSpeed
                                    * 0.1

                    else
                        model.eyePoint
            in
            ( { model | eyePoint = position }, Cmd.none )

        KeyChanged isDown key ->
            if key == "e" && model.isOptionOpen == False then
                ( { model | isOptionOpen = True, isViewMoveEnable = False }, exitPointerLock () )
                -- else if key == "Escape" then
                --     ( { model | isOptionOpen = True, isViewMoveEnable = False }, exitPointerLock () )

            else
                ( { model | keyStatus = updateKeyStatus isDown key model.keyStatus }, Cmd.none )

        DisableIsClick ->
            ( { model | isClick = False }, Cmd.none )

        ViewMove dx dy ->
            let
                dxFloat =
                    Pixels.toFloat dx * model.option.viewMoveSpeed * 0.1

                dyFloat =
                    Pixels.toFloat dy * model.option.viewMoveSpeed * 0.1

                angle2D =
                    -(dxFloat / 100) * model.option.viewMoveSpeed * 0.1

                angle3D =
                    dyFloat / 100 * model.option.viewMoveSpeed * 0.1

                newFocalVector =
                    if model.isViewMoveEnable == True then
                        { x = model.focalVector.x * cos angle2D - model.focalVector.y * sin angle2D
                        , y = model.focalVector.x * sin angle2D + model.focalVector.y * cos angle2D
                        , z = clamp -1.0 1.0 model.focalVector.z - angle3D
                        }

                    else
                        model.focalVector
            in
            ( { model | focalVector = newFocalVector }, Cmd.none )

        DrawPoints dx dy ->
            if model.isClick == True then
                ( { model
                    | points =
                        model.points
                            ++ [ { coord =
                                    addPoints model.eyePoint <|
                                        multiplePoints model.focalVector model.option.penDistance
                                 , color = model.option.penColor
                                 , size = model.option.penSize
                                 }
                               ]
                    , numPoints = model.numPoints + 1
                  }
                , Cmd.none
                )

            else
                ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    let
        ( firstLight, _ ) =
            pointLight
                { position = Point3d.centimeters -50 -30 70
                , chromaticity = Light.incandescent
                , intensity = LuminousFlux.lumens 500
                }

        ( secondLight, _ ) =
            pointLight
                { position = Point3d.centimeters -40 10 90
                , chromaticity = Light.fluorescent
                , intensity = LuminousFlux.lumens 500
                }

        thirdLight =
            Light.directional (Light.castsShadows True)
                { direction = Direction3d.xyZ (Angle.degrees -90) (Angle.degrees -45)
                , chromaticity =
                    Light.colorTemperature <|
                        Temperature.kelvins 2000
                , intensity = Illuminance.lux 30
                }

        softLighting =
            Light.soft
                { upDirection = Direction3d.positiveZ
                , chromaticity = Light.fluorescent
                , intensityAbove = Illuminance.lux 30
                , intensityBelow = Illuminance.lux 5
                }

        floor =
            Scene3d.quad (Material.matte Color.lightGreen)
                (Point3d.meters -200 -200 0)
                (Point3d.meters 200 -200 0)
                (Point3d.meters 200 200 0)
                (Point3d.meters -200 200 0)

        camera =
            Camera3d.perspective
                { viewpoint =
                    Viewpoint3d.lookAt
                        { eyePoint = Point3d.meters model.eyePoint.x model.eyePoint.y model.eyePoint.z
                        , focalPoint =
                            convertToPoint3d <|
                                addPoints model.focalVector model.eyePoint
                        , upDirection = Direction3d.positiveZ
                        }
                , verticalFieldOfView = Angle.degrees 30
                }

        explainText =
            pre
                [ style "position" "absolute"
                , style "top" "10%"
                , style "left" "10%"
                , style "background-color" "#fff"
                , style "transform" "translate(-50%, -50%)"
                , style "padding" "20px"
                , style "z-index" "9999"
                , style "font-weight" "bold"
                , style "font-size" "15px"
                , style "font-family" "Arial, sans-serif"
                ]
                [ Html.text "マウスクリックで操作開始\n"
                , Html.text "Escで操作解除\n"
                , Html.text "WASDで移動\n"
                , Html.text "Space/Shiftで上昇/下降\n"
                , Html.text "クリック+マウス移動で点を描画\n"
                , Html.text "Eでオプション表示"
                ]

        hud =
            div
                [ style "position" "absolute"
                , style "top" "50%"
                , style "left" "50%"
                , style "font-size" "24px"
                ]
                [ Html.text "+" ]

        optionModal =
            if model.isOptionOpen == True then
                div
                    [ style "position" "absolute"
                    , style "top" "50%"
                    , style "left" "50%"
                    , style "background-color" "#fff"
                    , style "transform" "translate(-50%, -50%)"
                    , style "padding" "20px"
                    , style "z-index" "9999"
                    , style "font-weight" "bold"
                    , style "font-size" "24px"
                    , style "font-family" "Arial, sans-serif"
                    ]
                    [ div []
                        [ Html.text "移動速度\u{3000}"
                        , Html.input
                            [ type_ "range"
                            , Html.Attributes.min "1"
                            , Html.Attributes.max "100"
                            , value (String.fromFloat model.option.moveSpeed)
                            , onInput
                                (\newValue ->
                                    ChangeMoveSpeed
                                        (String.toFloat newValue
                                            |> Maybe.withDefault 0.0
                                        )
                                )
                            ]
                            []
                        ]
                    , div []
                        [ Html.text "視点移動速度\u{3000}"
                        , Html.input
                            [ type_ "range"
                            , Html.Attributes.min "1"
                            , Html.Attributes.max "30"
                            , value (String.fromFloat model.option.viewMoveSpeed)
                            , onInput
                                (\newValue ->
                                    ChangeViewMoveSpeed
                                        (String.toFloat newValue
                                            |> Maybe.withDefault 0.0
                                        )
                                )
                            ]
                            []
                        ]
                    , div []
                        [ Html.text "ペンの太さ\u{3000}"
                        , Html.input
                            [ type_ "range"
                            , Html.Attributes.min "1"
                            , Html.Attributes.max "1000"
                            , value (String.fromFloat model.option.penSize)
                            , onInput
                                (\newValue ->
                                    ChangePenSize
                                        (String.toFloat newValue
                                            |> Maybe.withDefault 0.0
                                        )
                                )
                            ]
                            []
                        ]
                    , div []
                        [ Html.text "ペンの色\u{3000}"
                        , Html.input
                            [ type_ "color"
                            , value model.option.penColor
                            , onInput ChangePenColor
                            ]
                            []
                        ]
                    , div []
                        [ Html.text "ペンの距離\u{3000}"
                        , Html.input
                            [ type_ "range"
                            , Html.Attributes.min "0"
                            , Html.Attributes.max "100"
                            , value (String.fromFloat model.option.penDistance)
                            , onInput
                                (\newValue ->
                                    ChangePenDistance
                                        (String.toFloat newValue
                                            |> Maybe.withDefault 0.0
                                        )
                                )
                            ]
                            []
                        ]
                    , div [] [ Html.button [ onMouseDown CloseOptionModal, style "font-size" "24px" ] [ Html.text "Close" ] ]
                    ]

            else
                div []
                    []
    in
    div
        [ id "canvas"
        , onMouseDown RequestPointerLock
        , onMouseUp DisableIsClick
        ]
        [ Scene3d.custom
            { entities = [ floor ] ++ createSphereEntities model.points model.numPoints
            , lights = Scene3d.fourLights firstLight secondLight thirdLight softLighting
            , camera = camera
            , background = Scene3d.backgroundColor Color.lightBlue
            , clipDepth = Length.centimeters 10
            , exposure = Scene3d.exposureValue 6
            , toneMapping = Scene3d.hableFilmicToneMapping
            , whiteBalance = Light.fluorescent
            , antialiasing = Scene3d.multisampling
            , dimensions = ( model.width, model.height )
            }
        , explainText
        , hud
        , optionModal
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Browser.Events.onResize (\width height -> ResizeWindow (Pixels.int width) (Pixels.int height))
        , Browser.Events.onAnimationFrameDelta TimeDelta
        , Browser.Events.onMouseMove drawPointsDecoder
        , Browser.Events.onMouseMove viewMoveDecoder
        , Browser.Events.onKeyUp (Decode.map (KeyChanged False) (Decode.field "key" Decode.string))
        , Browser.Events.onKeyDown (Decode.map (KeyChanged True) (Decode.field "key" Decode.string))
        ]



-- FUNCTIONS


updateKeyStatus : Bool -> String -> KeyStatusModel -> KeyStatusModel
updateKeyStatus isDown key keys =
    case key of
        "w" ->
            { keys | up = isDown }

        "a" ->
            { keys | left = isDown }

        "s" ->
            { keys | down = isDown }

        "d" ->
            { keys | right = isDown }

        " " ->
            { keys | space = isDown }

        "Shift" ->
            { keys | shift = isDown }

        _ ->
            keys


updateOption : OptionModel -> String -> String -> OptionModel
updateOption option select value =
    case select of
        "moveSpeed" ->
            { option | moveSpeed = Maybe.withDefault 0.0 <| String.toFloat value }

        "viewMoveSpeed" ->
            { option | viewMoveSpeed = Maybe.withDefault 0.0 <| String.toFloat value }

        "penSize" ->
            { option | penSize = Maybe.withDefault 0 <| String.toFloat value }

        "penColor" ->
            { option | penColor = value }

        "penDistance" ->
            { option | penDistance = Maybe.withDefault 0 <| String.toFloat value }

        _ ->
            option


createSphereEntities points n =
    let
        hexToRgb : String -> ( Float, Float, Float )
        hexToRgb hex =
            case String.dropLeft 1 hex of
                "ffffff" ->
                    ( 255.0, 255.0, 255.0 )

                "000000" ->
                    ( 0.0, 0, 0 )

                _ ->
                    ( 1.0, 0, 0 )

        createColorFromHex : String -> Color
        createColorFromHex hex =
            let
                ( r, g, b ) =
                    hexToRgb hex
            in
            Color.rgb r g b

        material =
            case List.head points of
                Just firstPoint ->
                    Material.nonmetal
                        { baseColor = createColorFromHex firstPoint.color
                        , roughness = 1.0
                        }

                Nothing ->
                    Material.nonmetal
                        { baseColor = Color.black
                        , roughness = 1.0
                        }

        sphereEntity =
            case List.head points of
                Just firstPoint ->
                    Scene3d.sphere material <|
                        Sphere3d.withRadius (Length.centimeters firstPoint.size) (Point3d.meters firstPoint.coord.x firstPoint.coord.y firstPoint.coord.z)

                Nothing ->
                    Scene3d.sphere material <|
                        Sphere3d.withRadius (Length.centimeters 0) Point3d.origin
    in
    if n > 0 then
        sphereEntity :: createSphereEntities (List.drop 1 points) (n - 1)

    else
        []


addPoints point1 point2 =
    { x = point1.x + point2.x, y = point1.y + point2.y, z = point1.z + point2.z }


minusPoints point1 point2 =
    { x = point1.x - point2.x, y = point1.y - point2.y, z = point1.z - point2.z }


multiplePoints point f =
    { x = point.x * f, y = point.y * f, z = point.z * f }


convertToPoint3d point =
    Point3d.meters point.x point.y point.z



-- DECODERS


viewMoveDecoder : Decoder Msg
viewMoveDecoder =
    Decode.oneOf
        [ Decode.map2
            ViewMove
            (Decode.field "movementX" (Decode.map Pixels.float Decode.float))
            (Decode.field "movementY" (Decode.map Pixels.float Decode.float))
        ]


drawPointsDecoder : Decoder Msg
drawPointsDecoder =
    Decode.oneOf
        [ Decode.map2
            DrawPoints
            (Decode.field "movementX" (Decode.map Pixels.float Decode.float))
            (Decode.field "movementY" (Decode.map Pixels.float Decode.float))
        ]


pointLight properties =
    let
        sphere =
            Sphere3d.atPoint properties.position (Length.millimeters 5)

        sphereLuminance =
            properties.intensity
                |> Quantity.per (SolidAngle.spats 0.5)
                |> Quantity.per (Sphere3d.surfaceArea sphere)

        sphereMaterial =
            Material.emissive properties.chromaticity sphereLuminance
    in
    ( Light.point (Light.castsShadows True) properties
    , Scene3d.sphere sphereMaterial sphere
    )
