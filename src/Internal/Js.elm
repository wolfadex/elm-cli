module Internal.Js exposing (..)

import Json.Decode exposing (Decoder)


type alias Error =
    { code : String
    , msg : String
    }


prettyError : Error -> String
prettyError err =
    "Error Code: " ++ err.code ++ ", " ++ err.msg


decodeJsResult : Decoder ok -> Decoder (Result Error ok)
decodeJsResult =
    decodeResult
        (Json.Decode.map2 Error
            (Json.Decode.field "code" Json.Decode.string)
            (Json.Decode.field "msg" Json.Decode.string)
        )


decodeJsResultString : Decoder ok -> Decoder (Result String ok)
decodeJsResultString =
    decodeResult (Json.Decode.field "msg" Json.Decode.string)


decodeResult : Decoder err -> Decoder ok -> Decoder (Result err ok)
decodeResult decodeErr decodeOk =
    Json.Decode.field "result" Json.Decode.string
        |> Json.Decode.andThen
            (\result ->
                case result of
                    "Ok" ->
                        Json.Decode.field "data" decodeOk
                            |> Json.Decode.map Ok

                    "Err" ->
                        Json.Decode.field "data" decodeErr
                            |> Json.Decode.map Err

                    _ ->
                        Json.Decode.fail "Result must be either Ok or Err"
            )

decodeMaybe : Decoder just -> Decoder (Maybe just)
decodeMaybe decodeJust =
    Json.Decode.field "result" Json.Decode.string
        |> Json.Decode.andThen
            (\maybe ->
                case maybe of
                    "Just" ->
                        Json.Decode.field "data" decodeJust
                            |> Json.Decode.map Just

                    "Nothing" ->
                        Json.Decode.succeed Nothing

                    _ ->
                        Json.Decode.fail "Maybe must be either Just or Nothing"
            )
