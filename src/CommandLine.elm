module CommandLine exposing (fuzzyScore, fuzzyMatch)

import Char


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

                rest = if match then iRest else input

                (restScore, restMatches) = fuzzyScore tRest rest
            in
                 (score + restScore, match :: restMatches)

    in
        case Maybe.map2 maybeFunction (String.uncons target) (String.uncons input) of
            Just val ->
                val
            Nothing ->
                (0, [])



fuzzyMatch : (String -> String -> Int) -> List String -> String -> List String
fuzzyMatch scoringFunction options query =
    let
        withScores =
            List.map2 (\x y -> (x,y)) options
                <| List.map 
                    (\option -> scoringFunction option query) options
    in
        List.map (\(string, _) -> string)
            <| List.sortBy (\(_, score) -> score) withScores
