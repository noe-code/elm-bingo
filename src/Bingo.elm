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
                |> Html.text
    in
    Html.h2 [ id "info", class "classy" ]
        [ playerInfoText ]


main =
    viewPlayer "Noe" 3
