module Internal.Process exposing (..)

import Json.Decode exposing (Decoder)
import Json.Encode exposing (Value)
import Task exposing (Task)


type alias PosixProgram =
    PortIn Msg -> PortOut Msg -> Program Flags Model Msg



type alias Flags =
    { argv : List String
    , pid : Int
    , env : Value
    }


type alias ArgsToJs =
    { fn : String
    , args : List Value
    }


type alias PortIn msg =
    (Value -> msg) -> Sub msg


type alias PortOut msg =
    ArgsToJs -> Cmd msg


type Model
    = WaitingForValue ArgsToJs (Decoder Eff)
    | WaitingForTask
    | Exited


type Msg
    = GotValue Value
    | GotNext Eff


type Eff
    = CallJs ArgsToJs (Decoder Eff)
    | PerformTask (Task Never Eff)
    | Done (Result String Int)


init : PortOut Msg -> Eff -> Flags -> ( Model, Cmd Msg )
init portOut initialEff _ =
    next portOut initialEff


update : PortOut Msg -> Msg -> Model -> ( Model, Cmd Msg )
update callJs msg model =
    case ( msg, model ) of
        ( GotValue value, WaitingForValue args decoder ) ->
            case Json.Decode.decodeValue decoder value of
                Ok eff ->
                    next callJs eff

                Err err ->
                    ( Exited
                    , "Return value from javascript function '"
                        ++ args.fn
                        ++ "' could not be decoded:\n"
                        ++ Json.Decode.errorToString err
                        |> panic
                        |> callJs
                    )

        ( GotNext eff, WaitingForTask ) ->
            next callJs eff

        _ ->
            ( Exited
            , panic "This should never happen"
                |> callJs
            )


next : PortOut Msg -> Eff -> ( Model, Cmd Msg )
next callJs eff =
    case eff of
        CallJs args decoder ->
            ( WaitingForValue args decoder, callJs args )

        PerformTask task ->
            ( WaitingForTask
            , Task.perform GotNext task
            )

        Done result ->
            case result of
                Ok status ->
                    ( Exited
                    , { fn = "exit", args = [ Json.Encode.int status ] }
                        |> callJs
                    )

                Err err ->
                    ( Exited
                    , { fn = "panic", args = [ Json.Encode.string err ] }
                        |> callJs
                    )


subscriptions : PortIn Msg -> Model -> Sub Msg
subscriptions portIn _ =
    portIn GotValue


makeProgram : Eff -> PosixProgram
makeProgram initialEff portIn portOut =
    Platform.worker
        { init = init portOut initialEff
        , update = update portOut
        , subscriptions = subscriptions portIn
        }


panic : String -> ArgsToJs
panic msg =
    { fn = "panic"
    , args =
        [ Json.Encode.string msg
        ]
    }
