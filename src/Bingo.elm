module Bingo exposing (main)

import Html exposing (..)
import Html.Attributes exposing (..)


playerInfo : String -> Int -> String
playerInfo name gameNumber =
    name ++ " - Game# " ++ String.fromInt gameNumber


playerInfoText : String -> Int -> Html.Html msg
playerInfoText name gameNumber =
    playerInfo name gameNumber
        |> String.toUpper
        |> Html.text


viewPlayer : String -> Int -> Html.Html msg
viewPlayer name gameNumber =
    Html.h2 [ id "info", class "classy" ]
        [ playerInfoText name gameNumber ]


main =
    viewPlayer "Noe" 3
