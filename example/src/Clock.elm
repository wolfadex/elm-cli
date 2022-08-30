module Clock exposing (..)

{-| Prints the time on the terminal every second.
-}

import IO exposing (IO)
import Time


{-| This is the entry point, you can think of it as `main` in normal Elm applications.
-}
program : IO String ()
program =
    IO.printLn "Will print the current time 3 times and then exit."
        |> IO.and (IO.performTask Time.here)
        |> IO.andThen (\zone -> printTime zone 3)


printTime : Time.Zone -> Int -> IO String ()
printTime zone count =
    IO.performTask Time.now
        |> IO.map (toHHMMSS zone)
        |> IO.andThen IO.printLn
        |> IO.and
            (if count <= 1 then
                IO.none

             else
                IO.sleep 1000
                    |> IO.and (printTime zone (count - 1))
            )


toHHMMSS : Time.Zone -> Time.Posix -> String
toHHMMSS zone posix =
    let
        pad n =
            String.fromInt n
                |> String.padLeft 2 '0'

        h =
            Time.toHour zone posix

        m =
            Time.toMinute zone posix

        s =
            Time.toSecond zone posix
    in
    [ pad h
    , pad m
    , pad s
    ]
        |> String.join ":"
