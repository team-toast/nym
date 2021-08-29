module Demo exposing (main)

import Angle
import Browser
import Camera3d
import Color
import Direction3d
import Element exposing (Element)
import Eth.Types exposing (Address)
import Eth.Utils
import Hex
import Html exposing (Html)
import Length
import List.Extra
import Nym
import Pixels
import Point3d
import Random
import Scene3d
import Scene3d.Material as Material
import Svg
import Svg.Attributes as SvgA
import Viewpoint3d


main : Html msg
main =
    let
        -- Create a single rectangle from its color and four vertices
        -- (Scene3d.quad can be used to create any flat four-sided shape)
        square =
            Scene3d.quad (Material.color Color.blue)
                Nym.testStructure.innerBrow
                Nym.testStructure.outerBrow
                Nym.testStructure.eyecheek
                Nym.testStructure.eyenose

        -- Create a camera using perspective projection
        camera =
            Camera3d.perspective
                { -- Camera is at the point (4, 2, 2), looking at the point
                  -- (0, 0, 0), oriented so that positive Z appears up
                  viewpoint =
                    Viewpoint3d.lookAt
                        { focalPoint = Point3d.origin
                        , eyePoint = Point3d.meters 0 0 3
                        , upDirection = Direction3d.positiveY
                        }

                -- The image on the screen will have a total rendered 'height'
                -- of 30 degrees; small angles make the camera act more like a
                -- telescope and large numbers make it act more like a fisheye
                -- lens
                , verticalFieldOfView = Angle.degrees 30
                }
    in
    -- Render a scene that doesn't involve any lighting (no lighting is needed
    -- here since we provided a material that will result in a constant color
    -- no matter what lighting is used)
    Element.layout
        [ Element.width Element.fill
        , Element.height Element.fill
        ]
    <|
        Element.el [ Element.centerX, Element.centerY ] <|
            Element.html <|
                Scene3d.unlit
                    { -- Our scene has a single 'entity' in it
                      entities = [ square ]

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



-- type alias Msg =
--     ()
-- main =
--     Browser.sandbox
--         { init = ()
--         , view = always view
--         , update = always always ()
--         }
-- generator : Random.Generator Char
-- generator =
--     Random.int 0 15
--         |> Random.map Hex.toString
--         |> Random.map String.toList
--         |> Random.map List.head
--         |> Random.map (Maybe.withDefault '0')
-- testEthAddresses : List Address
-- testEthAddresses =
--     List.range 0 100
--         |> List.map Random.initialSeed
--         |> List.map randomEthAddr
-- randomEthAddr : Random.Seed -> Address
-- randomEthAddr seed =
--     let
--         getNextChar : ( Int, Random.Seed ) -> Maybe ( Char, ( Int, Random.Seed ) )
--         getNextChar ( at, seed_ ) =
--             if at < 40 then
--                 Just <|
--                     let
--                         ( char, newSeed ) =
--                             Random.step generator seed_
--                     in
--                     ( char, ( at + 1, newSeed ) )
--             else
--                 Nothing
--     in
--     List.Extra.unfoldr getNextChar ( 0, seed )
--         |> String.fromList
--         |> Eth.Utils.unsafeToAddress
-- view : Html Msg
-- view =
--     Element.layout [] <|
--         Element.wrappedRow
--             [ Element.width Element.fill
--             ]
--             (List.map
--                 viewPhaceForAddress
--                 testEthAddresses
--             )
-- viewPhaceForAddress : Address -> Element ()
-- viewPhaceForAddress address =
--     Element.el [ Element.padding 10 ] <|
--         Element.html <|
--             Phace.fromEthAddress address
