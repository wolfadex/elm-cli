module IO.Http exposing (..)

import IO exposing (IO)
import Json.Encode exposing (Value)
import Json.Decode
import Internal.Js
import Json.Decode exposing (Decoder)
import Internal.Process as Proc exposing (Eff)


type Request = Request Internal


type alias Internal =
    Value

createServer : { port_ : Int, host : String } -> IO String Request
createServer options =
    Internal.Js.decodeJsResult (Json.Decode.map Request Json.Decode.value)
        |> Json.Decode.map (Result.mapError Internal.Js.prettyError)
        |> IO.callJs "http:createServer"
            [ Json.Encode.object
                [ ( "port", Json.Encode.int options.port_ )
                , ( "host", Json.Encode.string options.host )
                ]
            ]
        |> IO.andThen IO.fromResult


respondWith : Int -> String -> Request -> IO String ()
respondWith statusCode content (Request request) =
    Json.Decode.succeed ()
        |> IO.callJs "http:respond"
            [ request
            , Json.Encode.int statusCode
            , Json.Encode.string content
            ]


path : Request -> IO x String
path (Request request) =
    Json.Decode.string
        |> IO.callJs "http:getpath" [ request ]