module PaginatedTests exposing (..)

import Dict
import Expect
import Paginated
import Paginated.Util
import Test exposing (..)


suite : Test
suite =
    describe "Paginated"
        [ test "Parse links" <|
            \() ->
                let
                    header =
                        String.join ", "
                            [ "<https://gitlab.example.com/api/v4/projects/8/issues/8/notes?page=1&per_page=3>; rel=\"prev\""
                            , "<https://gitlab.example.com/api/v4/projects/8/issues/8/notes?page=3&per_page=3>; rel=\"next\""
                            , "<https://gitlab.example.com/api/v4/projects/8/issues/8/notes?page=1&per_page=3>; rel=\"first\""
                            , "<https://gitlab.example.com/api/v4/projects/8/issues/8/notes?page=3&per_page=3>; rel=\"last\""
                            ]

                    expected =
                        Dict.fromList
                            [ ( "prev", "https://gitlab.example.com/api/v4/projects/8/issues/8/notes?page=1&per_page=3" )
                            , ( "next", "https://gitlab.example.com/api/v4/projects/8/issues/8/notes?page=3&per_page=3" )
                            , ( "first", "https://gitlab.example.com/api/v4/projects/8/issues/8/notes?page=1&per_page=3" )
                            , ( "last", "https://gitlab.example.com/api/v4/projects/8/issues/8/notes?page=3&per_page=3" )
                            ]
                in
                    Expect.equalDicts expected (Paginated.Util.links header)
        ]
