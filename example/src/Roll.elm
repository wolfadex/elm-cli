module Roll exposing (..)

{-| Rolls a random number.
-}

import IO exposing (IO)
import IO.Random
import Random
import System.Environment


{-| This is the entry point, you can think of it as `main` in normal Elm applications.
-}
program : IO String ()
program =
    System.Environment.getArgs
        |> IO.map (List.head >> Maybe.andThen String.toInt >> Maybe.withDefault 100)
        |> IO.andThen (\highLimit -> IO.Random.generate (Random.int 1 highLimit) |> IO.map (Tuple.pair highLimit))
        |> IO.andThen
            (\( highLimit, randomNumber ) ->
                IO.printLn (String.fromInt randomNumber ++ " (" ++ String.fromInt highLimit ++ ")")
            )
