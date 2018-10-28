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


main =
    -- Html.h2 : List (Attribute msg) -> List (Html msg) -> Html msg
    Html.h2 [ id "info", class "classy" ] [ playerInfoText "Noe" 5 ]
