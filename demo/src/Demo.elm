module Demo exposing (main)

import Angle
import Axis3d
import Browser
import Camera3d
import Color
import Direction3d
import Element exposing (Element)
import Element.Events
import Eth.Types exposing (Address)
import Eth.Utils
import Hex
import Html exposing (Html)
import Html.Events
import Json.Decode as Decode
import Length
import List.Extra
import Mouse
import Nym
import Pixels
import Point2d exposing (Point2d, xCoordinate, yCoordinate)
import Point3d
import Quantity
import Random
import Scene3d
import Scene3d.Material as Material
import Svg
import Svg.Attributes as SvgA
import Viewpoint3d


type Msg
    = MouseMove Mouse.MoveData


type alias MouseInput =
    { x : Float
    , y : Float
    }


type alias Model =
    { mouseInput : MouseInput }


main : Program () Model Msg
main =
    Browser.sandbox
        { init =
            Model
                (MouseInput 0 0)
        , view = view
        , update = update
        }


update : Msg -> Model -> Model
update msg model =
    case msg of
        MouseMove moveData ->
            { model
                | mouseInput =
                    MouseInput
                        (toFloat moveData.offsetX / moveData.offsetWidth - 0.5)
                        (toFloat moveData.offsetY / moveData.offsetHeight - 0.5)
            }


view : Model -> Html Msg
view model =
    let
        camera =
            Camera3d.perspective
                { viewpoint =
                    Viewpoint3d.lookAt
                        { focalPoint = Point3d.origin
                        , eyePoint =
                            Point3d.xyz
                                (Length.meters 0)
                                (Length.meters 0)
                                (Length.meters 3)
                                |> Point3d.rotateAround
                                    Axis3d.y
                                    (Angle.degrees (-model.mouseInput.x * 120))
                                |> Point3d.rotateAround
                                    Axis3d.x
                                    (Angle.degrees (-model.mouseInput.y * 120))
                        , upDirection = Direction3d.positiveY
                        }
                , verticalFieldOfView = Angle.degrees 30
                }
    in
    Element.layout
        [ Element.width Element.fill
        , Element.height Element.fill
        , Element.htmlAttribute <|
            Html.Events.on
                "mousemove"
                (Decode.map MouseMove Mouse.moveDecoder)
        ]
    <|
        Element.el [ Element.centerX, Element.centerY ] <|
            Element.html <|
                Scene3d.unlit
                    { -- Our scene has a single 'entity' in it
                      entities = [ Nym.makeNymEntity Nym.testNym ]

                    -- Provide the camera to be used when rendering the scene
                    , camera = camera

                    -- Anything closer than 1 meter to the camera will be clipped away
                    -- (this is necessary because of the internals of how WebGL works)
                    , clipDepth = Length.meters 1

                    -- Using a transparent background means that the HTML underneath the
                    -- scene will show through
                    , background = Scene3d.transparentBackground

                    -- Size in pixels of the generated HTML element
                    , dimensions = ( Pixels.int 400, Pixels.int 300 )
                    }
