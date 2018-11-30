module Main exposing (main)

import Browser
import Debug exposing (toString)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as Decode exposing (Decoder, field, int, string, succeed)
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import Json.Encode as Encode
import List
import Random
import ViewHelpers exposing (..)


type GameState
    = EnteringName
    | Playing


type alias Model =
    { name : String
    , game : Int
    , entries : List Entry
    , alertMessage : Maybe String
    , inputName : String
    , gameState : GameState
    }


type alias Entry =
    { id : Int
    , phrase : String
    , points : Int
    , marked : Bool
    }


type alias Score =
    { id : Int
    , name : String
    , score : Int
    }



--MODEL


initialModel : Model
initialModel =
    { name = "Anonymous"
    , game = 2
    , entries = []
    , alertMessage = Nothing
    , inputName = ""
    , gameState = EnteringName
    }



-- UPDATE


type Msg
    = NewGame
    | Mark Int
    | Sort
    | NewRandom Int
    | NewEntries (Result Http.Error (List Entry))
    | CloseAlertMessage
    | ShareScore
    | NewScore (Result Http.Error Score)
    | SetNameInput String
    | SaveName
    | CancelName
    | ChangeGameState GameState


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeGameState state ->
            ( { model | gameState = state }, Cmd.none )

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
            ( { model | alertMessage = Just (httpErrorToMessage error) }, Cmd.none )

        ShareScore ->
            ( model, postScore model )

        NewScore (Ok score) ->
            let
                message =
                    "Your score of "
                        ++ Debug.toString score.score
                        ++ " was successfully shared"
            in
            ( { model | alertMessage = Just message }, Cmd.none )

        NewScore (Err error) ->
            let
                message =
                    "Error posting your score"
                        ++ Debug.toString error
            in
            ( { model | alertMessage = Just message }, Cmd.none )

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

        SetNameInput value ->
            ( { model | inputName = value }, Cmd.none )

        SaveName ->
            if String.isEmpty model.inputName then
                ( model, Cmd.none )

            else
                ( { model | name = model.inputName, gameState = Playing, inputName = "" }, Cmd.none )

        CancelName ->
            ( { model | inputName = "", gameState = Playing }, Cmd.none )


httpErrorToMessage : Http.Error -> String
httpErrorToMessage error =
    case error of
        Http.NetworkError ->
            "Is the server running?"

        Http.BadStatus response ->
            Debug.toString response.status

        Http.BadPayload response _ ->
            "Decoding Failed: " ++ response

        _ ->
            Debug.toString error



-- DECODERS / ENCODERS
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


encodeScore : Model -> Encode.Value
encodeScore model =
    Encode.object
        [ ( "name", Encode.string model.name )
        , ( "score", Encode.int (sumMarkedPoints model.entries) )
        ]


scoreDecoder : Decoder Score
scoreDecoder =
    Decode.map3 Score
        (field "id" Decode.int)
        (field "name" Decode.string)
        (field "score" Decode.int)



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


postScore : Model -> Cmd Msg
postScore model =
    let
        postUrl =
            "http://localhost:3000/scores"

        body =
            encodeScore model
                |> Http.jsonBody

        request =
            Http.post postUrl body scoreDecoder
    in
    Http.send NewScore request



-- VIEW


viewPlayer : Model -> Html Msg
viewPlayer model =
    h2 [ id "info", class "classy" ]
        [ a [ href "#", onClick (ChangeGameState EnteringName) ] [ text model.name ]
        , text (" - Game# " ++ String.fromInt model.game)
        ]


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


zeroPoints : Model -> Bool
zeroPoints model =
    sumMarkedPoints model.entries == 0



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


viewNameInput : Model -> Html Msg
viewNameInput model =
    case model.gameState of
        EnteringName ->
            div [ class "name-input" ]
                [ input
                    [ type_ "Text"
                    , placeholder "Who's playing?"
                    , autofocus True
                    , value model.inputName
                    , onInput SetNameInput
                    ]
                    []
                , button [ onClick SaveName ] [ text "Save" ]
                , button [ onClick CancelName ] [ text "Cancel" ]
                ]

        Playing ->
            text ""


view : Model -> Html Msg
view model =
    div [ class "content" ]
        [ viewHeader "BUZZWORD BINGO"
        , viewPlayer model
        , viewNameInput model
        , alert CloseAlertMessage model.alertMessage
        , viewEntryList model.entries
        , viewScore model.entries
        , div [ class "button-group" ]
            [ button [ onClick NewGame ] [ text "New Game" ]
            , button [ onClick Sort ] [ text "Sort" ]
            , button [ onClick ShareScore, disabled (zeroPoints model) ] [ text "Share Score" ]
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
