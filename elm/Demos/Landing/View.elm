module Demos.Landing.View exposing (..)

import Demos.Common
import Demos.ElementHelpers as EH exposing (DisplayProfile(..), responsiveVal)
import Demos.Landing.Theme exposing (italic, newTabLink, normalText)
import Demos.Landing.Types exposing (..)
import Demos.Morph
import Element exposing (Attribute, Element, column, el, fill, padding, paddingEach, paragraph, px, row, shrink, spacing)
import Element.Background as Background
import Element.Font as Font
import Html exposing (Html)
import Json.Decode as Decode
import Mouse
import Nym
import Vector2 exposing (Vector2)


view : Model -> Html Msg
view model =
    let
        dProfile =
            Desktop
    in
    Element.layout
        [ Element.width fill
        , Element.height Element.shrink
        , Background.color <| Element.rgb 0.1 0.1 0.1
        ]
    <|
        el
            (responsiveVal dProfile
                [ paddingEach
                    { top = 50
                    , bottom = 0
                    , right = 20
                    , left = 20
                    }
                , Element.width <| (fill |> Element.maximum 1000)
                , Element.centerX
                ]
                [ padding 10
                , Element.width fill
                ]
            )
        <|
            body dProfile model


body : DisplayProfile -> Model -> Element Msg
body dProfile model =
    Element.column
        [ spacing 15
        ]
        (List.map
            (paragraph
                [ spacing 2
                , Element.width fill
                ]
            )
            [ [ normalText "Nyms combine two trends in the NFT space: PFP (profile pic) projects like "
              , newTabLink
                    { url = "https://boredapeyachtclub.com/"
                    , text = "Bored Apes"
                    }
              , normalText " and generative NFTs like "
              , newTabLink
                    { url = "https://tylerxhobbs.com/fidenza"
                    , text = "Fidenza"
                    }
              , normalText ". As a PFP project, Nyms are intended to be used as unique a social avatar, easily recognizable as an "
              , italic "individual"
              , normalText " on various social platforms. But as a generative NFT project, each Nym has a uniqueness that goes far above the typical PFP project's method of simply layering a limited set features onto a template."
              ]
            , [ normalText "Instead, each Nym's structuring and color is generated bottom-up from a random data source, with constraints designed to target a mammal-like head while leaving a lot of room for variety. This results in a set of more strikingly visually distinct items, and drastically increases the possibility space of the set."
              ]
            , [ viewMorphDemo model.morphModel
                    |> Element.map MorphMsg
              ]
            , [ normalText "INSERT SIDE-BY-SIDE COMPARISON" ]
            , [ normalText "Oh, and they're 3D too, which has some exciting implications for use in the Metaverse." ]
            , [ normalText "INSERT NYMS ZOOMING AROUND?" ]
            ]
        )



-- compareBaycToNyms : DisplayProfile -> MorphingModel -> Element Msg
-- compareBaycToNyms dProfile morphingModel =
--     let
--         width =
--             400
--     in
--     Element.row
--         [ Element.centerX
--         , Element.width shrink
--         , Element.height shrink
--         ]
--         [ morphingNym width morphingModel
--         , baycGif
--             [ Element.width <| px width
--             , Element.height <| px width
--             ]
--         ]


viewMorphDemo : Demos.Morph.Model -> Element Demos.Morph.Msg
viewMorphDemo morphModel =
    el
        [ Element.width <| px 500 ]
    <|
        Demos.Common.viewNymWithPixelDimensions
            ( 500, 500 )
            ( "100%", "100%" )
            morphModel.laggedMouse
            (Demos.Common.interpolateNymsForRendering
                morphModel.morphProgress
                morphModel.oldNymTemplate
                morphModel.newNymTemplate
                |> Nym.renderNymTemplate False
            )
            (Decode.map Demos.Morph.MouseMove Mouse.moveDecoder)



-- morphingNyms : MorphingNymsModel -> Element Msg
-- morphingNyms morphingNymsModel =
--     el
--         [ Element.width <| px width
--         , Element.height <| px width
--         ]
--     <|
--         let
--             widthStr =
--                 String.fromInt width ++ "px"
--         in
--         Demos.Common.viewNymWithPixelDimensions
--             ( width, width )
--             ( widthStr, widthStr )
--             (Vector2 0.5 0.5)
--             (Demos.Common.interpolateNymsForRendering
--                 morphingModel.morphProgress
--                 morphingModel.oldNym
--                 morphingModel.newNym
--                 |> Nym.renderNymTemplate False
--             )
--             (Decode.succeed NoOp)
-- baycGif : List (Attribute Msg) -> Element Msg
-- baycGif attributes =
--     Element.image
--         attributes
--         { src = "http://jingculturecommerce.com/wp-content/uploads/2021/09/sothebys-bored-apes-2.gif"
--         , description = "Bored Apes Cycling Gif"
--         }
