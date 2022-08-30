module HttpServer exposing (..)

{-| Example of reading a stream.

Read a CSV file line by line and convert each row to a JSON object.

-}

import Json.Encode as Encode
import IO exposing (IO)
import IO.Http exposing (Request)


{-| This is the entry point, you can think of it as `main` in normal Elm applications.
-}
program : IO String ()
program =
    IO.combine
        [ IO.Http.createServer { port_ = 4321, host = "127.0.0.1" }
            |> IO.andThen handler
        , IO.printLn "Listening at: localhost:4321"
        ]
        |> IO.and IO.forever


handler : Request -> IO String ()
handler request =
    request
        |> IO.Http.path
            |> IO.andThen
                (\path ->
                    IO.Http.respondWith 200 ("Hello " ++ path) request
                )