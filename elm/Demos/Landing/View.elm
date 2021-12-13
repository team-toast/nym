module Demos.Landing.View exposing (..)

import Demos.ElementHelpers as EH exposing (DisplayProfile(..), responsiveVal)
import Demos.Landing.Types exposing (..)
import Element exposing (Element, column, el, fill, height, padding, paddingEach, paragraph, row, spacing, width)
import Element.Background as Background
import Element.Font as Font
import Html exposing (Html)


view : Model -> Html Msg
view model =
    let
        dProfile =
            Desktop
    in
    Element.layout
        [ width fill
        , height fill
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
                , width <| (fill |> Element.maximum 1000)
                , Element.centerX
                ]
                [ padding 10
                , width fill
                ]
            )
        <|
            justAllTheTextEl dProfile


justAllTheTextEl : DisplayProfile -> Element Msg
justAllTheTextEl dProfile =
    Element.column
        [ spacing 15
        ]
        (List.map
            (paragraph
                [ spacing 2
                , width fill
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
              , normalText " on various social platforms. But as a generative NFT project, each Nym has a uniqueness that goes far above the typical PFP project's method of simply stacking artwork of various \"traits\" on top of one another."
              ]
            , [ normalText "To compare Nyms versus Bored Apes (the clear apex PFP project today), each set clearly has a theme. But if you had one of each - which would feel more like "
              , italic "yours?"
              , normalText " Which would have more individual, immediately recognizable "
              , italic "character"
              , normalText " when used on a social or governance platform?"
              ]
            , [ normalText "INSERT SIDE-BY-SIDE COMPARISON" ]
            , [ normalText "Oh, and they're 3D too, which has some exciting implications for use in the Metaverse." ]
            , [ normalText "INSERT NYMS ZOOMING AROUND?" ]
            ]
        )


normalText : String -> Element Msg
normalText s =
    el
        [ Font.color <| Element.rgb 1 1 1 ]
    <|
        Element.text s


italic : String -> Element Msg
italic =
    el [ Font.italic ] << normalText


newTabLink : { url : String, text : String } -> Element Msg
newTabLink d =
    Element.newTabLink
        []
        { url = d.url
        , label =
            Element.el
                [ Font.color <| Element.rgb 0.4 0.4 1
                ]
            <|
                Element.text d.text
        }
