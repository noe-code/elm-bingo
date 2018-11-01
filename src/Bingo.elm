module Bingo exposing (main)

import Browser
import Debug exposing (toString)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import List exposing (..)


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
    , { id = 3, prhase = "block chain", points = 600, marked = False }
    , { id = 4, prhase = "the cloud", points = 100, marked = False }
    ]



-- UPDATE


type Msg
    = NewGame
    | Mark Int
    | Sort


update : Msg -> Model -> Model
update msg model =
    case msg of
        NewGame ->
            { model | game = model.game + 1, entries = initialEntries }

        Mark id ->
            let
                markEntry e =
                    if e.id == id then
                        { e | marked = not e.marked }

                    else
                        e
            in
            { model | entries = List.map markEntry model.entries }

        Sort ->
            let
                sortedEntries entries =
                    reverse (sortBy .points entries)
            in
            { model | entries = sortedEntries model.entries }



-- VIEW


playerInfo : String -> Int -> String
playerInfo name gameNumber =
    name ++ " - Game# " ++ String.fromInt gameNumber


viewPlayer : String -> Int -> Html Msg
viewPlayer name gameNumber =
    let
        playerInfoText =
            playerInfo name gameNumber
                |> String.toUpper
                |> text
    in
    h2 [ id "info", class "classy" ]
        [ playerInfoText ]


viewHeader : String -> Html Msg
viewHeader title =
    header []
        [ h1
            []
            [ text title ]
        ]


viewEntryItem : Entry -> Html Msg
viewEntryItem entry =
    li [ classList [ ( "marked", entry.marked ) ], onClick (Mark entry.id) ]
        [ span [ class "prhase" ] [ text entry.prhase ]
        , span [ class "points" ] [ text (String.fromInt entry.points) ]
        ]


viewEntryList : List Entry -> Html Msg
viewEntryList entries =
    let
        listOfEntries =
            List.map viewEntryItem entries
    in
    ul [] listOfEntries


viewFooter : Html Msg
viewFooter =
    footer []
        [ a [ href "https://elm-lang.org/" ]
            [ text "Powered by Elm" ]
        ]


view : Model -> Html Msg
view model =
    div [ class "content" ]
        [ viewHeader "BUZZWORD BINGO"
        , viewPlayer model.name model.game
        , viewEntryList model.entries
        , div [ class "button-group" ]
            [ button [ onClick NewGame ] [ text "New Game" ]
            ]
        , div [ class "button-group" ]
            [ button [ onClick Sort ] [ text "Sort" ]
            ]
        , div [ class "debug" ] [ text (Debug.toString model) ]
        , viewFooter
        ]


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }
