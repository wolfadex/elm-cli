module HelloUser exposing (..)

{-| Prints the user name on the terminal.

-}
import Dict exposing (Dict)
import Posix.IO as IO exposing (IO)


{-| This is the entry point, you can think of it as `main` in normal Elm applications.
-}
program : IO String ()
program  =
    IO.getEnv "USER"
        |> IO.andThen
            (\maybeUser ->
                case maybeUser of
                    Nothing ->
                        IO.printLn "Hello stranger"

                    Just user ->
                        IO.printLn ("Hello " ++ user)
            )



