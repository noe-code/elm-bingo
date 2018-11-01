module Bingo exposing (main)

import Browser
import Debug exposing (toString)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import List


type alias Model =
    { name : String
    , game : Int
    , entries : List Entry
    }


type alias Entry =
    { id : Int
    , phrase : String
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
    [ { id = 1, phrase = "holistic", points = 400, marked = False }
    , { id = 2, phrase = "sinergy", points = 200, marked = False }
    , { id = 3, phrase = "block chain", points = 600, marked = False }
    , { id = 4, phrase = "the cloud", points = 100, marked = False }
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
                    List.reverse (List.sortBy .points entries)
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
        [ span [ class "phrase" ] [ text entry.phrase ]
        , span [ class "points" ] [ text (String.fromInt entry.points) ]
        ]


viewEntryList : List Entry -> Html Msg
viewEntryList entries =
    let
        listOfEntries =
            List.map viewEntryItem entries
    in
    ul [] listOfEntries


sumMarkedPoints : List Entry -> Int
sumMarkedPoints entries =
    entries
        |> List.filter .marked
        |> List.map .points
        |> List.sum



{- sumMarkedPoints : List Entry -> Int
   sumMarkedPoints entries =
       entries
           |> List.filter .marked
           |> List.foldl (\e sum -> sum + e.points) 0
-}


viewScore : List Entry -> Html Msg
viewScore entries =
    div [ class "score" ]
        [ span [ class "label" ] [ text "Score" ]
        , span [ class "value" ] [ text (String.fromInt (sumMarkedPoints entries)) ]
        ]


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
        , viewScore model.entries
        , div [ class "button-group" ]
            [ button [ onClick NewGame ] [ text "New Game" ]
            , button [ onClick Sort ] [ text "Sort" ]
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
