module CommandLine exposing (fuzzyScore, fuzzyMatch, Command(..), parseCommand, ParseError(..), ParamGreed (..))

import Char

type Command a
    = Terminal a
    | NonTerminal ParamGreed (List String) (String -> Maybe (Command a))


type ParseError a
    = InvalidParameter
    | MissingParameters (Command a)
    | ExtraParameters


type ParamGreed
    = Word
    | Rest


{-|
  Separates a String into Just (firstWord, rest) if the string
  is non-empty or Nothing if it is empty
-}
separateFirstWord : String -> Maybe (String, String)
separateFirstWord query =
    let
        words = String.words query
        first = List.head words
        rest = List.tail words
    in
        case (first, rest) of
            (Just first, Just rest) -> Just (first, String.join " " rest)
            _ -> Nothing

parseCommand : String -> Command a -> Result (ParseError a) a
parseCommand query command =
    case command of
        Terminal val ->
            case query of
                "" ->
                    Ok val
                _ ->
                    Err ExtraParameters
        NonTerminal greed suggestions parsingFunction ->
            let
                splitQuery =
                    case greed of
                        Word ->
                            separateFirstWord query
                        Rest ->
                            Just (query, "")
            in
                case splitQuery of
                    Just (param, rest) ->
                        case (parsingFunction param) of
                            Just command ->
                                parseCommand rest command
                            Nothing ->
                                Err InvalidParameter
                    Nothing ->
                        Err (MissingParameters command)


{-
fuzzyParseCommand : String -> Command a -> Result (ParseError a) a
fuzzyParseCommand query command =
    case command of
        Terminal val ->
            case query of
                "" ->
                    Ok val
                _ ->
                    Err ExtraParameters
        NonTerminal greed suggestions parsingFunction ->
            let

            in
            case suggestions of
                [] -> 
-}


fuzzyScore : String -> String -> (Int, List Bool)
fuzzyScore target input =
    let
        maybeFunction : (Char, String) -> (Char, String) -> (Int, List Bool)
        maybeFunction (t, tRest) (i, iRest) =
            let
                (score, match) =
                    if Char.toLower t == Char.toLower i then
                        (1, True)
                    else
                        (0, False)

                restInput = if match then iRest else input

                (restScore, restMatches) = fuzzyScore tRest restInput
            in
                (score + restScore, match :: restMatches)
    in
        case Maybe.map2 maybeFunction (String.uncons target) (String.uncons input) of
            Just val ->
                val
            Nothing ->
                (0, List.repeat (String.length target) False)



fuzzyMatch :
    (String -> String -> (Int, List Bool))
    -> List String
    -> String
    -> List (String, List Bool)
fuzzyMatch scoringFunction options query =
    let
        withScores =
            List.map2 (\x y -> (x,y)) options
                <| List.map 
                    (\option -> scoringFunction option query) options
    in
        List.map (\(string, (_, matched)) -> (string, matched))
            <| List.sortBy (\(_, (score, _)) -> score) withScores
