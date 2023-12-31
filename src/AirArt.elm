port module AirArt exposing (main)

import Angle
import Block3d exposing (Block3d)
import Browser
import Browser.Dom
import Browser.Events
import Camera3d
import Color exposing (Color)
import Cylinder3d exposing (Cylinder3d)
import Direction3d exposing (Direction3d)
import Frame3d
import Hex
import Html exposing (Html, div, pre)
import Html.Attributes exposing (id, style, type_, value)
import Html.Events exposing (onInput, onMouseDown, onMouseUp)
import Illuminance
import Json.Decode as Decode exposing (Decoder)
import Length
import LuminousFlux
import Pixels
import Point3d exposing (Point3d)
import Quantity exposing (Quantity)
import Random
import Scene3d
import Scene3d.Light as Light exposing (Chromaticity, Light)
import Scene3d.Material as Material
import SolidAngle
import Sphere3d
import String exposing (left)
import Task
import Temperature
import Viewpoint3d


port requestPointerLock : () -> Cmd msg


port exitPointerLock : () -> Cmd msg



-- #     #    #    ### #     #
-- ##   ##   # #    #  ##    #
-- # # # #  #   #   #  # #   #
-- #  #  # #     #  #  #  #  #
-- #     # #######  #  #   # #
-- #     # #     #  #  #    ##
-- #     # #     # ### #     #


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- ### #     # ### #######
--  #  ##    #  #     #
--  #  # #   #  #     #
--  #  #  #  #  #     #
--  #  #   # #  #     #
--  #  #    ##  #     #
-- ### #     # ###    #


init : () -> ( Model, Cmd Msg )
init () =
    ( { width = Quantity.zero
      , height = Quantity.zero
      , randomNumbersList = []
      , eyePoint = { x = 0.0, y = 0.0, z = 2.0 }
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
      , isStartModalOpen = True
      , isExplainTextOpen = False
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



-- #     # ####### ######  ####### #
-- ##   ## #     # #     # #       #
-- # # # # #     # #     # #       #
-- #  #  # #     # #     # #####   #
-- #     # #     # #     # #       #
-- #     # #     # #     # #       #
-- #     # ####### ######  ####### #######


type alias Model =
    { width : Quantity Int Pixels.Pixels
    , height : Quantity Int Pixels.Pixels
    , randomNumbersList : List Int
    , eyePoint : Point3dModel
    , focalVector : Point3dModel
    , keyStatus : KeyStatusModel
    , isClick : Bool
    , points : List PointModel
    , numPoints : Int
    , isStartModalOpen : Bool
    , isExplainTextOpen : Bool
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



-- #     #  #####   #####
-- ##   ## #     # #     #
-- # # # # #       #
-- #  #  #  #####  #  ####
-- #     #       # #     #
-- #     # #     # #     #
-- #     #  #####   #####


type Msg
    = ResizeWindow (Quantity Int Pixels.Pixels) (Quantity Int Pixels.Pixels)
    | RequestPointerLock
    | UpdateModel Int
    | KeyChanged Bool String
    | DisableIsClick
    | TimeDelta Float
    | ViewMove (Quantity Float Pixels.Pixels) (Quantity Float Pixels.Pixels)
    | OpenOptionModal
    | CloseOptionModal
    | ChangeMoveSpeed Float
    | ChangeViewMoveSpeed Float
    | ChangePenSize Float
    | ChangePenColor String
    | ChangePenDistance Float



-- #     # ######  ######     #    ####### #######
-- #     # #     # #     #   # #      #    #
-- #     # #     # #     #  #   #     #    #
-- #     # ######  #     # #     #    #    #####
-- #     # #       #     # #######    #    #
-- #     # #       #     # #     #    #    #
--  #####  #       ######  #     #    #    #######


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ResizeWindow width height ->
            ( { model | width = width, height = height }, Cmd.none )

        RequestPointerLock ->
            if model.isOptionOpen == False then
                ( { model | isClick = True, isStartModalOpen = False, isExplainTextOpen = True, isOptionOpen = False, isViewMoveEnable = True }, requestPointerLock () )

            else
                ( model, Cmd.none )

        UpdateModel randomNumber ->
            ( { model | randomNumbersList = model.randomNumbersList ++ [ randomNumber ] }
            , Cmd.none
            )

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
                    if model.keyStatus.up == True && model.isStartModalOpen == False then
                        addPoints model.eyePoint <|
                            multiplePoints model.focalVector <|
                                model.option.moveSpeed
                                    * 0.1

                    else if model.keyStatus.down == True && model.isStartModalOpen == False then
                        minusPoints model.eyePoint <|
                            multiplePoints model.focalVector <|
                                model.option.moveSpeed
                                    * 0.1

                    else if model.keyStatus.left == True && model.isStartModalOpen == False then
                        addPoints model.eyePoint <|
                            multiplePoints toRightVector <|
                                model.option.moveSpeed
                                    * 0.1

                    else if model.keyStatus.right == True && model.isStartModalOpen == False then
                        addPoints model.eyePoint <|
                            multiplePoints toLeftVector <|
                                model.option.moveSpeed
                                    * 0.1

                    else if model.keyStatus.space == True && model.isStartModalOpen == False then
                        addPoints model.eyePoint <|
                            multiplePoints toUpVector <|
                                model.option.moveSpeed
                                    * 0.1

                    else if model.keyStatus.shift == True && model.isStartModalOpen == False then
                        addPoints model.eyePoint <|
                            multiplePoints toDownVector <|
                                model.option.moveSpeed
                                    * 0.1

                    else
                        model.eyePoint
            in
            if model.isClick == True then
                let
                    cmd =
                        if List.length model.randomNumbersList < 500 then
                            Random.generate UpdateModel (Random.int -200 200)

                        else
                            Cmd.none
                in
                ( { model
                    | eyePoint = position
                    , points =
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
                , cmd
                )

            else
                let
                    cmd =
                        if List.length model.randomNumbersList < 500 then
                            Random.generate UpdateModel (Random.int -200 200)

                        else
                            Cmd.none
                in
                ( { model | eyePoint = position }, cmd )

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



-- #     # ### ####### #     #
-- #     #  #  #       #  #  #
-- #     #  #  #       #  #  #
-- #     #  #  #####   #  #  #
--  #   #   #  #       #  #  #
--   # #    #  #       #  #  #
--    #    ### #######  ## ##


view : Model -> Html Msg
view model =
    let
        lightBulb =
            Light.point (Light.castsShadows True)
                { chromaticity = Light.incandescent
                , intensity = LuminousFlux.lumens 300
                , position = Point3d.meters 0 0 300
                }

        overheadLighting =
            Light.overhead
                { upDirection = Direction3d.positiveZ
                , chromaticity = Light.fluorescent
                , intensity = Illuminance.lux 100
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

        startModal =
            if model.isStartModalOpen == True then
                pre
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
                    [ Html.text "マウスで自由にお絵かきしよう！\n"
                    , Html.text "オプションでペンの太さや色を変えられるよ！\n"

                    -- , Html.text "書いた線を掴んで移動させることもできる！\n"
                    , Html.text "好きなところをクリックしてスタート！！！\n"
                    ]

            else
                div []
                    []

        explainText =
            if model.isExplainTextOpen == True then
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

            else
                div []
                    []

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
            { entities = [ floor ] ++ createTreeEntities model ++ createCloudEntities ++ createSphereEntities model.points model.numPoints
            , lights = Scene3d.twoLights lightBulb overheadLighting
            , camera = camera
            , background = Scene3d.backgroundColor Color.lightBlue
            , clipDepth = Length.centimeters 10
            , exposure = Scene3d.exposureValue 6
            , toneMapping = Scene3d.noToneMapping
            , whiteBalance = Light.fluorescent
            , antialiasing = Scene3d.multisampling
            , dimensions = ( model.width, model.height )
            }
        , explainText
        , hud
        , startModal
        , optionModal
        ]



--  #####  #     # ######   #####   #####  ######  ### ######  ####### ### ####### #     #  #####
-- #     # #     # #     # #     # #     # #     #  #  #     #    #     #  #     # ##    # #     #
-- #       #     # #     # #       #       #     #  #  #     #    #     #  #     # # #   # #
--  #####  #     # ######   #####  #       ######   #  ######     #     #  #     # #  #  #  #####
--       # #     # #     #       # #       #   #    #  #          #     #  #     # #   # #       #
-- #     # #     # #     # #     # #     # #    #   #  #          #     #  #     # #    ## #     #
--  #####   #####  ######   #####   #####  #     # ### #          #    ### ####### #     #  #####


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Browser.Events.onResize (\width height -> ResizeWindow (Pixels.int width) (Pixels.int height))
        , Browser.Events.onAnimationFrameDelta TimeDelta
        , Browser.Events.onMouseMove viewMoveDecoder
        , Browser.Events.onKeyUp (Decode.map (KeyChanged False) (Decode.field "key" Decode.string))
        , Browser.Events.onKeyDown (Decode.map (KeyChanged True) (Decode.field "key" Decode.string))
        ]



-- ####### #     # #     #  #####  ####### ### ####### #     #  #####
-- #       #     # ##    # #     #    #     #  #     # ##    # #     #
-- #       #     # # #   # #          #     #  #     # # #   # #
-- #####   #     # #  #  # #          #     #  #     # #  #  #  #####
-- #       #     # #   # # #          #     #  #     # #   # #       #
-- #       #     # #    ## #     #    #     #  #     # #    ## #     #
-- #        #####  #     #  #####     #    ### ####### #     #  #####


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
        hexToRgb hex =
            let
                red =
                    case Hex.fromString (String.slice 1 3 hex) of
                        Ok intValue ->
                            toFloat intValue / 255

                        Err _ ->
                            0.0

                green =
                    case Hex.fromString (String.slice 3 5 hex) of
                        Ok intValue ->
                            toFloat intValue / 255

                        Err _ ->
                            0.0

                blue =
                    case Hex.fromString (String.slice 5 7 hex) of
                        Ok intValue ->
                            toFloat intValue / 255

                        Err _ ->
                            0.0
            in
            Color.rgb red green blue

        material =
            case List.head points of
                Just firstPoint ->
                    Material.nonmetal
                        { baseColor = hexToRgb firstPoint.color
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


createTreeEntities model =
    let
        treeCenters =
            randomNumbersList2Coords model.randomNumbersList <| List.length model.randomNumbersList

        trunkMaterial =
            Material.metal
                { baseColor = Color.brown
                , roughness = 1.0
                }

        leavesMaterial =
            Material.metal
                { baseColor = Color.green
                , roughness = 1.0
                }

        createTrunkEntity centersList n =
            let
                trunkEntity =
                    case List.head centersList of
                        Just center ->
                            Scene3d.cylinder trunkMaterial (Cylinder3d.centeredOn (convertToPoint3d center) Direction3d.z { length = Length.meters 3.0, radius = Length.meters 0.25 })

                        _ ->
                            Scene3d.cylinder trunkMaterial (Cylinder3d.centeredOn (Point3d.meters 0 0 0) Direction3d.z { length = Length.meters 0, radius = Length.meters 0 })

                leavesEntity =
                    case List.head centersList of
                        Just center ->
                            Scene3d.sphere leavesMaterial
                                (Sphere3d.withRadius (Length.meters 1.0) <|
                                    convertToPoint3d { x = center.x, y = center.y, z = center.z + 1.5 }
                                )

                        _ ->
                            Scene3d.sphere leavesMaterial <|
                                Sphere3d.withRadius (Length.meters 0) Point3d.origin
            in
            if n > 2 then
                [ trunkEntity, leavesEntity ] ++ createTrunkEntity (List.drop 2 centersList) (n - 1)

            else
                [ trunkEntity, leavesEntity ]
    in
    createTrunkEntity treeCenters <|
        List.length treeCenters


createCloudEntities =
    let
        cloudDimensions =
            [ ( Length.meters 40, Length.meters 40, Length.meters 2 )
            , ( Length.meters 20, Length.meters 40, Length.meters 2 )
            , ( Length.meters 30, Length.meters 50, Length.meters 2 )
            , ( Length.meters 35, Length.meters 20, Length.meters 2 )
            , ( Length.meters 40, Length.meters 60, Length.meters 2 )
            , ( Length.meters 60, Length.meters 30, Length.meters 2 )
            , ( Length.meters 35, Length.meters 40, Length.meters 2 )
            , ( Length.meters 25, Length.meters 50, Length.meters 2 )
            , ( Length.meters 40, Length.meters 30, Length.meters 2 )
            , ( Length.meters 70, Length.meters 70, Length.meters 2 )
            , ( Length.meters 60, Length.meters 40, Length.meters 2 )
            , ( Length.meters 30, Length.meters 55, Length.meters 2 )
            , ( Length.meters 50, Length.meters 40, Length.meters 2 )
            , ( Length.meters 45, Length.meters 75, Length.meters 2 )
            , ( Length.meters 60, Length.meters 50, Length.meters 2 )
            , ( Length.meters 25, Length.meters 55, Length.meters 2 )
            , ( Length.meters 55, Length.meters 60, Length.meters 2 )
            , ( Length.meters 50, Length.meters 25, Length.meters 2 )
            , ( Length.meters 25, Length.meters 60, Length.meters 2 )
            , ( Length.meters 45, Length.meters 65, Length.meters 2 )
            ]

        cloudBoxCenters =
            [ Point3d.meters 50 50 100
            , Point3d.meters 300 -100 100
            , Point3d.meters -150 30 100
            , Point3d.meters -50 200 100
            , Point3d.meters 80 -170 100
            , Point3d.meters 250 300 100
            , Point3d.meters 400 200 100
            , Point3d.meters -50 450 100
            , Point3d.meters -350 -90 100
            , Point3d.meters -450 400 100
            , Point3d.meters 500 500 100
            , Point3d.meters 500 -500 100
            , Point3d.meters -500 500 100
            , Point3d.meters -500 -500 100
            , Point3d.meters 50 -450 100
            , Point3d.meters -150 -300 100
            , Point3d.meters 350 -400 100
            , Point3d.meters -250 250 100
            , Point3d.meters -400 100 100
            , Point3d.meters 450 50 100
            ]

        cloudMaterial =
            Material.metal
                { baseColor = Color.white
                , roughness = 1.0
                }

        createCloudEntity dimensionsList centersList n =
            let
                cloudEntity =
                    case ( List.head centersList, List.head dimensionsList ) of
                        ( Just center, Just dimension ) ->
                            Scene3d.blockWithShadow cloudMaterial (Block3d.centeredOn (Frame3d.atPoint center) dimension)

                        _ ->
                            Scene3d.blockWithShadow cloudMaterial (Block3d.centeredOn (Frame3d.atPoint (Point3d.meters 0 0 0)) ( Length.meters 0, Length.meters 0, Length.meters 0 ))
            in
            if n > 1 then
                [ cloudEntity ] ++ createCloudEntity (List.drop 1 dimensionsList) (List.drop 1 centersList) (n - 1)

            else
                [ cloudEntity ]
    in
    createCloudEntity cloudDimensions cloudBoxCenters <|
        List.length cloudDimensions


addPoints point1 point2 =
    { x = point1.x + point2.x, y = point1.y + point2.y, z = point1.z + point2.z }


minusPoints point1 point2 =
    { x = point1.x - point2.x, y = point1.y - point2.y, z = point1.z - point2.z }


multiplePoints point f =
    { x = point.x * f, y = point.y * f, z = point.z * f }


convertToPoint3d point =
    Point3d.meters point.x point.y point.z


randomNumbersList2Coords randomNumbersList n =
    let
        randomNumbersX =
            case List.head randomNumbersList of
                Just randomNumber ->
                    randomNumber

                Nothing ->
                    0

        randomNumbersY =
            case List.head (List.drop 1 randomNumbersList) of
                Just randomNumber ->
                    randomNumber

                Nothing ->
                    0

        coords =
            { x = toFloat randomNumbersX, y = toFloat randomNumbersY, z = 1.5 }
    in
    if n > 1 then
        [ coords ] ++ randomNumbersList2Coords (List.drop 1 randomNumbersList) (n - 2)

    else
        [ coords ]



-- ######  #######  #####  ####### ######  ####### ######
-- #     # #       #     # #     # #     # #       #     #
-- #     # #       #       #     # #     # #       #     #
-- #     # #####   #       #     # #     # #####   ######
-- #     # #       #       #     # #     # #       #   #
-- #     # #       #     # #     # #     # #       #    #
-- ######  #######  #####  ####### ######  ####### #     #


viewMoveDecoder : Decoder Msg
viewMoveDecoder =
    Decode.oneOf
        [ Decode.map2
            ViewMove
            (Decode.field "movementX" (Decode.map Pixels.float Decode.float))
            (Decode.field "movementY" (Decode.map Pixels.float Decode.float))
        ]
