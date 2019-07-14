module Main exposing (..)

import Html exposing (..)
import Html
import Html.Events exposing (onInput)
import Browser

import CommandLine
import CommandLine exposing (Command (..), ParamGreed (..), FuzzyError(..))

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


intParser : String -> Maybe Int
intParser word =
    String.toInt word

intReference : (Int -> CommandTest) -> Command CommandTest
intReference msg =
    NonTerminal Word []
        (\query ->
            case intParser query of
                Just val ->
                    Just <| Terminal (msg val)
                Nothing ->
                    Nothing
        )

groupReference : (Int -> Int -> CommandTest) -> Command CommandTest
groupReference msg =
    NonTerminal Word []
        (\query ->
            case intParser query of
                Just val ->
                    Just <| intReference (msg val)
                Nothing ->
                    Nothing
        )

topLevelCommand : List String -> Command CommandTest
topLevelCommand tags =
    NonTerminal Word ["hideUi", "toggleTag", "removeGroup"]
        (\query ->
            let
                tagCommand msg = NonTerminal Rest tags (\query_ -> Just (Terminal (msg query_)))

            in
                case query of
                    "hideUi" -> Just <| Terminal HideUi
                    "toggleTag" -> Just <| tagCommand ToggleTag
                    "removeGroup" -> Just <| groupReference RemoveGroup
                    _ -> Nothing
        )

type Msg
    = Input String


type alias Model =
    { suggestions: List (String, List Bool)
    , expandedQuery: String
    , commandResult: String
    }


init : flags -> (Model, Cmd Msg)
init _ =
    (Model [] "" "", Cmd.none)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Input query ->
            let
                (expanded, suggestions) =
                    CommandLine.expandCommand
                        query
                        (topLevelCommand ["yolo", "swag"])

                parsedCommand = CommandLine.parseCommand
                    query
                    <| topLevelCommand ["yolo", "swag"]

                commandResult =
                    case parsedCommand of
                        Ok HideUi -> 
                            "hideUi"
                        Ok (ToggleTag tag) ->
                            "toggleTag " ++ tag
                        Ok (RemoveGroup id1 id2) ->
                            "RemoveGroup " ++ (String.fromInt id1) ++ " " ++ String.fromInt id2
                        _ ->
                            ""
                newModel = case suggestions of
                    Ok suggestions_ ->
                        { model | suggestions = suggestions_ }
                    _ ->
                        model
            in
                ({newModel
                    | expandedQuery = expanded
                    , commandResult = commandResult
                }, Cmd.none)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Html.Html Msg
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
                    <| List.map2 Tuple.pair (String.toList string) matches
    in
    div
        []
        [ input [onInput Input] []
        , ul []
            <| List.map matchRenderer model.suggestions
        , p [] [text model.expandedQuery]
        , p [] [text model.commandResult]
        ]


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view = (\model -> {title = "Fuzzy command demo", body = [view model]})
        , subscriptions = subscriptions
        }
