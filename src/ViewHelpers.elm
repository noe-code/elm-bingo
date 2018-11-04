module ViewHelpers exposing (alert)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)


alert : msg -> Maybe String -> Html msg
alert msg alertMessage =
    case alertMessage of
        Just message ->
            div [ class "alert" ]
                [ div
                    []
                    [ text message ]
                , div
                    [ class "close", onClick msg ]
                    [ text "X" ]
                ]

        Nothing ->
            text ""
