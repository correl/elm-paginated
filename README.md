A library for fetching data from paginated JSON REST APIs.

# Motivation

This library is built to handle fetching data from a JSON REST API
that paginates its results. It inspects response headers, and will
prepare subsequent requests when additional pages of results remain.

Given a JSON decoder for the data type returned in the paginated
collection, it will return a data structure containing the results
fetched, and a new request object for fetching additional records from
the next page if more are available.

It is expected that the paginated REST API will provide a link for the
next page of results in the `Link` header, e.g.:

    Link: <https://api.example.com/search?q=abc&page=2>; rel="next"

Absence of a link with `rel="next"` in the response headers is assumed
to mean the final page of results has been reached.

# Example Usage

    import Http
    import Json.Decode exposing (string)
    import Paginated exposing (Response(..))

    type alias Model =
        { results : Maybe (List String) }

    type Msg
        = Search
        | Results (Result Http.Error (List String))

    update : Msg -> Model -> ( Model, Cmd Msg )
    update msg model =
        case msg of
            Search ->
                ( model, doSearch )

            Results results ->
                ( { model | results = Result.toMaybe results }
                , Cmd.none
                )

    doSearch : Cmd Msg
    doSearch =
        Paginated.send Results <|
            Paginated.get "http://example.com/search?q=abc" string
