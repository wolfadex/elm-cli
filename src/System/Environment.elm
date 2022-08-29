module System.Environment exposing
    ( getVariable
    , getArgs
    )

import IO exposing (IO)
import Internal.Js
import Json.Decode
import Json.Encode


getVariable : String -> IO x (Maybe String)
getVariable key =
    Internal.Js.decodeMaybe Json.Decode.string
        |> IO.callJs "getEnv" [ Json.Encode.string key ]


getArgs : IO x (List String)
getArgs =
    Json.Decode.list Json.Decode.string
        |> IO.callJs "getArgs" []