module Bingo exposing (main)

import Html exposing (..)
import Html.Attributes exposing (..)



--MODEL


initialModel =
    { name = "Dougie"
    , game = 2
    , entries = initialEntries
    }


initialEntries =
    [ { id = 1, prhase = "holistic", points = 400, marked = False }
    , { id = 2, prhase = "sinergy", points = 200, marked = False }
    ]



-- VIEW


playerInfo : String -> Int -> String
playerInfo name gameNumber =
    name ++ " - Game# " ++ String.fromInt gameNumber


viewPlayer : String -> Int -> Html msg
viewPlayer name gameNumber =
    let
        playerInfoText =
            playerInfo name gameNumber
                |> String.toUpper
                |> text
    in
    h2 [ id "info", class "classy" ]
        [ playerInfoText ]


viewHeader : String -> Html msg
viewHeader title =
    header []
        [ h1
            []
            [ text title ]
        ]


viewFooter : Html msg
viewFooter =
    footer []
        [ a [ href "https://elm-lang.org/" ]
            [ text "Powered by Elm" ]
        ]



--view : Html msg


view model =
    div [ class "content" ]
        [ viewHeader "BUZZWORD BINGO"
        , viewPlayer model.name model.game
        , viewFooter
        ]


main : Html msg
main =
    view initialModel
