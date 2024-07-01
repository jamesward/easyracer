module EasyRacer.Scenario10 exposing (main)

import EasyRacer.Ports as Ports
import Http
import Platform exposing (Program)
import Process
import Random
import Task
import Time


type alias Flags =
    String


type alias ClockTimes =
    { wall : Float
    , cpu : Float
    }


type Model
    = AwaitingId String -- baseUrl
    | Running
        { id : Int
        , baseUrl : String
        , keepBusy : Bool
        , lastClockTimes : Maybe ClockTimes
        }


type Msg
    = NewId Int
    | NewCpuUsage Ports.ClockTimes
    | BlockerHttpResponse (Result Http.Error String)
    | ReporterHttpResponse Ports.FetchResponse
    | BlockingStep ()
    | Perform (Cmd Msg)


scenarioPath : String
scenarioPath =
    "/10"


init : Flags -> ( Model, Cmd Msg )
init baseUrl =
    ( AwaitingId baseUrl
    , Random.int 0 Random.maxInt |> Random.generate NewId
    )


busyWait : Int -> ()
busyWait iteration =
    if iteration == 0 then
        ()

    else
        busyWait (iteration - 1)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( model, msg ) of
        ( AwaitingId baseUrl, NewId id ) ->
            ( Running
                { id = id
                , baseUrl = baseUrl
                , keepBusy = True
                , lastClockTimes = Nothing
                }
            , Cmd.batch
                [ Task.succeed (BlockingStep ()) |> Task.perform identity
                , Http.get
                    { url = baseUrl ++ scenarioPath ++ "?" ++ String.fromInt id
                    , expect = Http.expectString BlockerHttpResponse
                    }
                , Time.now
                    |> Task.andThen
                        (\now ->
                            let
                                wallTimeMicros : Int
                                wallTimeMicros =
                                    1000 * Time.posixToMillis now
                            in
                            Task.succeed (Ports.sendCpuUsageRequest wallTimeMicros)
                        )
                    |> Task.perform Perform
                ]
            )

        -- Blocker
        ( Running state, BlockerHttpResponse _ ) ->
            -- Stop, even if this is a failure response
            ( Running
                { state | keepBusy = False }
            , Cmd.none
            )

        -- Blocking
        ( Running state, BlockingStep _ ) ->
            ( model
            , if state.keepBusy then
                Process.sleep 0
                    |> Task.andThen (\_ -> Task.succeed (BlockingStep (busyWait 1000000)))
                    |> Task.perform identity

              else
                Cmd.none
            )

        -- Reporter
        -- Uses fetch for HTTP call, as Elm HTTP client doesn't allow manual redirect handling
        ( Running state, NewCpuUsage { wall, user, system } ) ->
            ( Running
                { state
                    | lastClockTimes = Just { wall = wall, cpu = user + system }
                }
            , case state.lastClockTimes of
                Just lastClockTimes ->
                    let
                        load : Float
                        load =
                            (user + system + lastClockTimes.cpu) / (wall + lastClockTimes.wall)

                        url : String
                        url =
                            state.baseUrl
                                ++ scenarioPath
                                ++ "?"
                                ++ String.fromInt state.id
                                ++ "="
                                ++ String.fromFloat load
                    in
                    Ports.sendFetchRequest url

                Nothing ->
                    Cmd.none
            )

        ( Running _, ReporterHttpResponse { statusCode, bodyText } ) ->
            ( model
            , case statusCode of
                200 ->
                    Ok bodyText |> Ports.sendResult

                302 ->
                    Process.sleep 1000
                        |> Task.andThen (\_ -> Time.now)
                        |> Task.andThen
                            (\now ->
                                let
                                    wallTimeMicros : Int
                                    wallTimeMicros =
                                        1000 * Time.posixToMillis now
                                in
                                Task.succeed (Ports.sendCpuUsageRequest wallTimeMicros)
                            )
                        |> Task.perform Perform

                _ ->
                    Err ("reporter received unexpected status " ++ String.fromInt statusCode) |> Ports.sendResult
            )

        -- Miscellaneous
        ( Running _, Perform cmd ) ->
            ( model, cmd )

        _ ->
            ( model, Err "Unexpected state" |> Ports.sendResult )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Ports.receiveCpuUsageResponse NewCpuUsage
        , Ports.receiveFetchResponse ReporterHttpResponse
        ]


main : Program Flags Model Msg
main =
    Platform.worker
        { init = init
        , update = update
        , subscriptions = subscriptions
        }
