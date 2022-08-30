module Internal.Process exposing (..)

import Dict exposing (Dict)
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
    ({ key : Int, data : Value } -> msg) -> Sub msg


type alias PortOut msg =
    ({ key: Int, data : ArgsToJs }) -> Cmd msg


type Model
    = Running RunningModel
    | Exited


type alias RunningModel =
    { continues : Dict Int Continue
    , nextId : Int
    }


type Continue
    = WaitingForValue ArgsToJs (Decoder Eff)
    | WaitingForTask


type Msg
    = GotValue { key : Int, data : Value }
    | GotNext { key : Int, data : Eff }


type Eff
    = CallJs ArgsToJs (Decoder Eff)
    | PerformTask (Task Never Eff)
    | Done (Result String Int)


init : PortOut Msg -> Eff -> Flags -> ( Model, Cmd Msg )
init portOut initialEff _ =
    next { nextId = 0, continues = Dict.empty} portOut initialEff


update : PortOut Msg -> Msg -> Model -> ( Model, Cmd Msg )
update callJs msg model =
    case ( msg, model ) of
        ( GotValue { key, data }, Running running ) ->
            case Dict.get key running.continues of
                Just (WaitingForValue args decoder) ->
                    case Json.Decode.decodeValue decoder data of
                        Ok eff ->
                            next running callJs eff

                        Err err ->
                            ( Exited
                            , callJs
                                { key = -1
                                , data =
                                    "Return value from javascript function '"
                                        ++ args.fn
                                        ++ "' could not be decoded:\n"
                                        ++ Json.Decode.errorToString err
                                        |> panic
                                }
                            )

                Just WaitingForTask ->
                    ( Exited
                    , callJs
                        { key = -1
                        , data = panic "This should never happen"
                        }
                    )

                Nothing ->
                    ( Exited
                    , callJs
                        { key = -1
                        , data = panic "This should never happen"
                        }
                    )

        ( GotNext { key, data }, Running running ) ->
            case Dict.get key running.continues of
                Just WaitingForTask ->
                    next running callJs data

                Just (WaitingForValue _ _) ->
                    ( Exited
                    , callJs
                        { key = -1
                        , data = panic "This should never happen"
                        }
                    )

                Nothing ->
                    ( Exited
                    , callJs
                        { key = -1
                        , data = panic "This should never happen"
                        }
                    )

        _ ->
            ( Exited
            , callJs
                { key = -1
                , data = panic "This should never happen"
                }
            )


next : RunningModel -> PortOut Msg -> Eff -> ( Model, Cmd Msg )
next runningModel callJs eff =
    case eff of
        CallJs args decoder ->
            ( Running
                { runningModel
                    | continues =
                        Dict.insert runningModel.nextId
                            (WaitingForValue args decoder)
                            runningModel.continues
                    , nextId = runningModel.nextId + 1
                }
            , callJs { key = runningModel.nextId, data = args }
            )

        PerformTask task ->
            ( Running
                { runningModel
                    | continues =
                        Dict.insert runningModel.nextId
                            WaitingForTask
                            runningModel.continues
                    , nextId = runningModel.nextId + 1
                }
            , Task.perform (\data -> GotNext { key = runningModel.nextId, data = data }) task
            )

        Done result ->
            case result of
                Ok status ->
                    ( Exited
                    , callJs
                        { key = -1
                        , data = { fn = "exit", args = [ Json.Encode.int status ] }
                        }
                    )

                Err err ->
                    ( Exited
                    , callJs
                        { key = -1
                        , data = { fn = "panic", args = [ Json.Encode.string err ] }
                        }
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
