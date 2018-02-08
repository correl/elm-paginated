module Paginated
    exposing
        ( Request
        , RequestOptions
        , request
        , get
        , post
        , send
        , toTask
        )

{-| A library for Facilitates fetching data from a paginated JSON API.


# Requests and Responses

@docs Request, get, post


## Custom requests

@docs RequestOptions, request


# Sending requests

@docs send


# Low-Level operations

@docs toTask

-}

import Char
import Dict exposing (Dict)
import Http
import Json.Decode exposing (Decoder)
import Paginated.Util
import Task exposing (Task)
import Time


{-| Describes an API request.
-}
type alias RequestOptions a =
    { method : String
    , headers : List Http.Header
    , url : String
    , body : Http.Body
    , decoder : Decoder a
    , timeout : Maybe Time.Time
    , withCredentials : Bool
    }


{-| Encapsulates an API request for a list of items of type `a`.
-}
type Request a
    = Request (RequestOptions a)


{-| Describes an API response.

A response may either be Partial (there are more pages of results yet
to be fetched), or Complete (all records have been fetched). The
response includes all of the items fetched in order.

-}
type Response a
    = Partial (Request a) (List a)
    | Complete (List a)


{-| Create a custom request, allowing the specification of HTTP
headers and other options. For example:

    Paginated.request
        { method = "GET"
        , headers = [Http.header "Private-Token" "XXXXXXXXXXXXXXXX"]
        , url = url
        , body = Http.emptyBody
        , decoder = decoder
        , timeout = Nothing
        , withCredentials = False
        }

-}
request : RequestOptions a -> Request a
request =
    Request


{-| Build a GET request.
-}
get : String -> Decoder a -> Request a
get url decoder =
    request
        { method = "GET"
        , headers = []
        , url = url
        , body = Http.emptyBody
        , decoder = decoder
        , timeout = Nothing
        , withCredentials = False
        }


{-| Build a POST request.
-}
post : String -> Http.Body -> Decoder a -> Request a
post url body decoder =
    request
        { method = "POST"
        , headers = []
        , url = url
        , body = body
        , decoder = decoder
        , timeout = Nothing
        , withCredentials = False
        }


{-| Send a `Request`.
-}
send :
    (Result Http.Error (List a) -> msg)
    -> Request a
    -> Cmd msg
send resultToMessage request =
    toTask request
        |> Task.attempt resultToMessage


{-| Convert a `Request` into a `Task`.

This is only really useful if you want to chain together a bunch of
requests (or any other tasks) in a single command.

-}
toTask : Request a -> Task Http.Error (List a)
toTask =
    httpRequest >> Http.toTask >> recurse


{-| Chains a paginated request task, fetching all available pages of
data.
-}
recurse :
    Task Http.Error (Response a)
    -> Task Http.Error (List a)
recurse =
    Task.andThen
        (\response ->
            case response of
                Partial request _ ->
                    httpRequest request
                        |> Http.toTask
                        |> Task.map (update response)
                        |> recurse

                Complete xs ->
                    Task.succeed xs
        )


{-| Append two paginated responses, collecting the results within.
-}
update : Response a -> Response a -> Response a
update old new =
    case ( old, new ) of
        ( Complete items, _ ) ->
            Complete items

        ( Partial _ oldItems, Complete newItems ) ->
            Complete (oldItems ++ newItems)

        ( Partial _ oldItems, Partial request newItems ) ->
            Partial request (oldItems ++ newItems)


{-| Convert a `Request` to a `Http.Request` that can then be sent via
`Http.send`.
-}
httpRequest : Request a -> Http.Request (Response a)
httpRequest (Request options) =
    Http.request
        { method = options.method
        , headers = options.headers
        , url = options.url
        , body = options.body
        , expect = expect options
        , timeout = options.timeout
        , withCredentials = options.withCredentials
        }


expect : RequestOptions a -> Http.Expect (Response a)
expect options =
    Http.expectStringResponse (fromResponse options)


fromResponse :
    RequestOptions a
    -> Http.Response String
    -> Result String (Response a)
fromResponse options response =
    let
        items : Result String (List a)
        items =
            Json.Decode.decodeString
                (Json.Decode.list options.decoder)
                response.body

        nextPage : Maybe String
        nextPage =
            header "Link" response.headers
                |> Maybe.map Paginated.Util.links
                |> Maybe.andThen (Dict.get "next")
    in
        case nextPage of
            Nothing ->
                Result.map Complete items

            Just url ->
                Result.map
                    (Partial (request { options | url = url }))
                    items


{-| Look up a header (case-insensitive)
-}
header : String -> Dict String String -> Maybe String
header header headers =
    let
        normalized =
            Dict.toList headers
                |> List.map (\( k, v ) -> ( String.map Char.toLower k, v ))
                |> Dict.fromList

        key =
            String.map Char.toLower header
    in
        Dict.get key normalized
