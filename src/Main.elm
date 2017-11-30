module Main exposing (..)

import Html exposing (..)
import Html
import Html.Events exposing (onInput)

import CommandLine
import CommandLine exposing (Command (..))

searchOptions : List String
searchOptions =
    [ "removeTag"
    , "toggleTag"
    , "toggleGroup"
    , "removeGroup"
    , "addTag"
    , "hideUi"
    ]


type CommandTest
    = RemoveTag String
    | ToggleTag String
    | RemoveGroup Int Int
    | HideUi


topLevelCommand : List String -> Command CommandTest
topLevelCommand tags =
    NonTerminal ["hideUi", "toggleTag", "removeTag", "removeGroup"]
        (\str ->
            let
                tagCommand tag = Just ("", ToggleTag tag)
                intCommand str =
                    Result.toMaybe <| String.toInt str
            in
                case str of
                    "hideUi" -> Terminal HideUi
                    "toggleTag" -> NonTerminal tags (Terminal tagCommand)
        )

type Msg =
    Input String


type alias Model =
    { queryResult: List (String, List Bool)}


init : (Model, Cmd Msg)
init =
    (Model [], Cmd.none)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Input query ->
            ({model | 
                queryResult =
                    CommandLine.fuzzyMatch
                        CommandLine.fuzzyScore
                        searchOptions
                        query
            }, Cmd.none)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Html Msg
view model =
    let
        matchRenderer : (String, List Bool) -> Html Msg
        matchRenderer (string, matches) =
            let
                charRenderer (char, match) =
                    ( if match then
                        b []
                      else
                        span []
                    ) [text <| String.fromChar char]
            in
            p
                []
                <| List.map charRenderer
                    <| List.map2 (,) (String.toList string) matches
    in
    div
        []
        [ input [onInput Input] []
        , ul []
            <| List.map matchRenderer model.queryResult
        ]


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
