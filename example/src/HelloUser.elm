module HelloUser exposing (..)

{-| Prints the user name on the terminal.

-}
import Dict exposing (Dict)
import IO exposing (IO)
import System.Environment


{-| This is the entry point, you can think of it as `main` in normal Elm applications.
-}
program : IO String ()
program  =
    System.Environment.getArgs
        |> IO.map (List.head >> Maybe.withDefault "USER")
        |> IO.andThen System.Environment.getVariable
        |> IO.andThen
            (\maybeUser ->
                case maybeUser of
                    Nothing ->
                        IO.printLn "Hello stranger"

                    Just user ->
                        IO.printLn ("Hello " ++ user)
            )



