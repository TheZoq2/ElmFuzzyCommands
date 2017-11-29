module CommandLine exposing (fuzzyScore, fuzzyMatch)

import Char


fuzzyScore : String -> String -> Int
fuzzyScore target input =
    let

        maybeFunction (t, tRest) (i, iRest) =
            let
                charFunction : Char -> Char -> (Int, String)
                charFunction tChar tInput =
                    if Char.toLower tChar == Char.toLower tInput then
                        (1, iRest)
                    else
                        (0, input)

                (charScore, rest) = 
                    (charFunction t i)
            in
                 charScore + fuzzyScore tRest rest

    in
        case Maybe.map2 maybeFunction (String.uncons target) (String.uncons input) of
            Just val ->
                val
            Nothing ->
                0



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
