module Bingo exposing (main)

import Browser
import Debug exposing (toString)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode exposing (Decoder, field, int, string, succeed)
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import List
import Random


type alias Model =
    { name : String
    , game : Int
    , entries : List Entry
    , alertMessage : Maybe String
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
    , entries = []
    , alertMessage = Nothing
    }



-- UPDATE


type Msg
    = NewGame
    | Mark Int
    | Sort
    | NewRandom Int
    | NewEntries (Result Http.Error (List Entry))
    | CloseAlertMessage


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewRandom randomNumber ->
            ( { model | game = randomNumber }, Cmd.none )

        NewGame ->
            ( { model | game = model.game + 1 }, getEntries )

        NewEntries (Ok randomEntries) ->
            let
                sortedEntries entries =
                    List.sortBy .points entries
            in
            ( { model | entries = sortedEntries randomEntries }, Cmd.none )

        --List.reverse (List.sortBy .points entries)
        NewEntries (Err error) ->
            let
                errorMessage =
                    case error of
                        Http.NetworkError ->
                            "Is the server running?"

                        Http.BadStatus response ->
                            Debug.toString response.status

                        Http.BadPayload response _ ->
                            "Decoding Failed: " ++ response

                        _ ->
                            Debug.toString error
            in
            ( { model | alertMessage = Just errorMessage }, Cmd.none )

        Mark id ->
            let
                markEntry e =
                    if e.id == id then
                        { e | marked = not e.marked }

                    else
                        e
            in
            ( { model | entries = List.map markEntry model.entries }, Cmd.none )

        Sort ->
            let
                sortedEntries entries =
                    List.reverse (List.sortBy .points entries)
            in
            ( { model | entries = sortedEntries model.entries }, Cmd.none )

        CloseAlertMessage ->
            ( { model | alertMessage = Nothing }, Cmd.none )



-- DECODERS
{- }
   entryDecoder : Decoder Entry
   entryDecoder =
       Decode.map4 Entry
           (field "id" Decode.int)
           (field "phrase" Decode.string)
           (field "points" Decode.int)
           (succeed False)

-}


entryDecoder : Decoder Entry
entryDecoder =
    succeed Entry
        |> required "id" int
        |> required "phrase" string
        |> optional "points" int 100
        |> hardcoded False



-- COMANDS


generateRandomNum : Cmd Msg
generateRandomNum =
    -- Random.generate (a ->msg) -> Generator a -> Cmd msg
    Random.generate NewRandom (Random.int 1 100)



-- getString: String -> Request String
-- send : (Result Error a -> msg) -> Request a -> Cmd msg
-- send : (Result Http.Error String -> Msg) -> Request String -> Cmd Msg


entriesUrl : String
entriesUrl =
    "http://localhost:3000/random-entries"



--Http.send NewEntries (Http.get entriesUrl (Decode.list entryDecoder))


getEntries : Cmd Msg
getEntries =
    Decode.list entryDecoder
        |> Http.get entriesUrl
        |> Http.send NewEntries



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


viewAlertMessage : Maybe String -> Html Msg
viewAlertMessage alertMessage =
    case alertMessage of
        Just message ->
            --div [ class "alert" ] [ text message ]
            div [ class "alert" ]
                [ div
                    []
                    [ text message ]
                , div
                    [ class "close", onClick CloseAlertMessage ]
                    [ text "X" ]
                ]

        Nothing ->
            text ""


view : Model -> Html Msg
view model =
    div [ class "content" ]
        [ viewHeader "BUZZWORD BINGO"
        , viewPlayer model.name model.game
        , viewAlertMessage model.alertMessage
        , viewEntryList model.entries
        , viewScore model.entries
        , div [ class "button-group" ]
            [ button [ onClick NewGame ] [ text "New Game" ]
            , button [ onClick Sort ] [ text "Sort" ]
            ]
        , div [ class "debug" ] [ text (Debug.toString model) ]
        , viewFooter
        ]



{- main : Program () Model Msg
   main =
       Browser.sandbox
           { init = initialModel
           , view = view
           , update = update
           }
-}


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel, getEntries )


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }
