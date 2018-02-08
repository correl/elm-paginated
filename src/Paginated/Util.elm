module Paginated.Util exposing (links)

import Dict exposing (Dict)
import Maybe.Extra
import Regex


{-| Parse an HTTP Link header into a dictionary. For example, to look
for a link to additional results in an API response, you could do the
following:

    Dict.get "Link" response.headers
        |> Maybe.map links
        |> Maybe.andThen (Dict.get "next")

-}
links : String -> Dict String String
links s =
    let
        toTuples xs =
            case xs of
                [ Just a, Just b ] ->
                    Just ( b, a )

                _ ->
                    Nothing
    in
        Regex.find Regex.All linkPattern s
            |> List.map .submatches
            |> List.map toTuples
            |> Maybe.Extra.values
            |> Dict.fromList


linkPattern : Regex.Regex
linkPattern =
    Regex.regex "<(.*?)>; rel=\"(.*?)\""
