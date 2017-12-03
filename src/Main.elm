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
        (\query ->
            let
                tagCommand = NonTerminal tags (\query -> Just ("", Terminal <| ToggleTag query))
                intCommand str =
                    Result.toMaybe <| String.toInt str

                words = String.words query
                (firstParam) = List.head words
                (restParams) = String.concat <| Maybe.withDefault [] <| List.tail words

                command =
                    case firstParam of
                        Just "hideUi" -> Just <| Terminal HideUi
                        Just "toggleTag" -> Just <| tagCommand
                        _ -> Nothing
            in
                case command of
                    Just command ->
                        Just (restParams, command)
                    Nothing ->
                        Nothing
        )

type Msg
    = Input String


type alias Model =
    { queryResult: List (String, List Bool)
    , commandResult: String
    }


init : (Model, Cmd Msg)
init =
    (Model [] "", Cmd.none)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Input query ->
            let
                fuzzyResult =
                    CommandLine.fuzzyMatch
                        CommandLine.fuzzyScore
                        searchOptions
                        query

                parsedCommand = CommandLine.parseCommand
                    query
                    <| topLevelCommand ["yolo", "swag"]

                commandResult =
                    case parsedCommand of
                        Ok HideUi -> 
                            "hideUi"
                        Ok (ToggleTag tag) ->
                            "toggleTag " ++ tag
                        _ ->
                            ""
            in
                ({model | 
                    queryResult = fuzzyResult,
                    commandResult = commandResult
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
        , p [] [text model.commandResult]
        ]


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
