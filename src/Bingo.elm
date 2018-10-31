module Bingo exposing (main)

import Debug exposing (toString)
import Html exposing (..)
import Html.Attributes exposing (..)
import List


type alias Model =
    { name : String
    , game : Int
    , entries : List Entry
    }


type alias Entry =
    { id : Int
    , prhase : String
    , points : Int
    , marked : Bool
    }



--MODEL


initialModel : Model
initialModel =
    { name = "Dougie"
    , game = 2
    , entries = initialEntries
    }


initialEntries : List Entry
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


viewEntryItem : Entry -> Html msg
viewEntryItem entries =
    li []
        [ span [ class "prhase" ] [ text entries.prhase ]
        , span [ class "points" ] [ text (String.fromInt entries.points) ]
        ]


viewEntryList : List Entry -> Html msg
viewEntryList entries =
    let
        listOfEntries =
            List.map viewEntryItem entries
    in
    ul [] listOfEntries


viewFooter : Html msg
viewFooter =
    footer []
        [ a [ href "https://elm-lang.org/" ]
            [ text "Powered by Elm" ]
        ]


view : Model -> Html msg
view model =
    div [ class "content" ]
        [ viewHeader "BUZZWORD BINGO"
        , viewPlayer model.name model.game
        , viewEntryList model.entries
        , div [ class "debug" ] [ text (Debug.toString model) ]
        , viewFooter
        ]


main : Html msg
main =
    view initialModel
