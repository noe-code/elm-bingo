module Bingo exposing (main)

import Html exposing (..)
import Html.Attributes exposing (..)


playerInfo : String -> Int -> String
playerInfo name gameNumber =
    name ++ " - Game# " ++ String.fromInt gameNumber


viewPlayer : String -> Int -> Html.Html msg
viewPlayer name gameNumber =
    let
        playerInfoText =
            playerInfo name gameNumber
                |> String.toUpper
                |> text
    in
    h2 [ id "info", class "classy" ]
        [ playerInfoText ]


viewHeader : String -> Html.Html msg
viewHeader title =
    header []
        [ text title ]


viewFooter : Html.Html msg
viewFooter =
    footer []
        [ a [ href "https://elm-lang.org/" ]
            [ text "Powered by Elm" ]
        ]


view : Html.Html msg
view =
    div [ class "content" ]
        [ viewHeader "BUZZWORD BINGO"
        , viewPlayer "Pepe" 2
        , viewFooter
        ]


main =
    view
