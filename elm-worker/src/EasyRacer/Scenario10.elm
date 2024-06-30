module EasyRacer.Scenario10 exposing (main)

import EasyRacer.Ports as Ports
import Http
import Platform exposing (Program)
import Process
import Random
import Task


type alias Flags =
    String


type Model
    = AwaitingId String -- baseUrl
    | Running
        { id : Int
        , baseUrl : String
        , keepBusy : Bool
        }


type ReporterResponse
    = Done String
    | Repeat


type Msg
    = NewId Int
    | NewCpuLoadPercent Float
    | BlockerHttpResponse (Result Http.Error String)
    | ReporterHttpResponse (Result Http.Error ReporterResponse)
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
    if iteration == 0 then ()
    else busyWait (iteration - 1)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( model, msg ) of
        ( AwaitingId baseUrl, NewId id ) ->
            ( Running
                { id = id
                , baseUrl = baseUrl
                , keepBusy = True
                }
            , Cmd.batch
                [ Task.succeed (BlockingStep ()) |> Task.perform identity
                , Http.get
                    { url = baseUrl ++ scenarioPath ++ "?" ++ String.fromInt id
                    , expect = Http.expectString BlockerHttpResponse
                    }
                , Ports.requestCpuLoadPercent ()
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
                Task.succeed (BlockingStep (busyWait 100000))
                    |> Task.perform identity

              else
                Cmd.none
            )

        -- Reporter
        ( Running state, NewCpuLoadPercent loadPercent ) ->
            let
                url : String
                url =
                    state.baseUrl
                        ++ scenarioPath
                        ++ "?"
                        ++ String.fromInt state.id
                        ++ "="
                        ++ String.fromFloat (loadPercent / 100.0)
            in
            ( model
            , Http.get
                { url = url
                , expect =
                    Http.expectStringResponse ReporterHttpResponse <|
                        \response ->
                            case response of
                                Http.BadUrl_ badUrl ->
                                    Err (Http.BadUrl badUrl)

                                Http.Timeout_ ->
                                    Err Http.Timeout

                                Http.NetworkError_ ->
                                    Err Http.NetworkError

                                Http.BadStatus_ metadata _ ->
                                    if metadata.statusCode /= 302 then
                                        Err (Http.BadStatus metadata.statusCode)

                                    else
                                        Ok Repeat

                                Http.GoodStatus_ _ body ->
                                    Ok <| Done body
                }
            )

        ( Running _, ReporterHttpResponse httpResult ) ->
            ( model
            , case httpResult of
                Ok (Done result) ->
                    Ok result |> Ports.sendResult

                Ok Repeat ->
                    Process.sleep 1000
                        |> Task.andThen (\_ -> Task.succeed (Perform (Ports.requestCpuLoadPercent ())))
                        |> Task.perform identity

                Err _ ->
                    Err "reporter received error" |> Ports.sendResult
            )

        -- Miscellaneous
        ( Running _, Perform cmd ) ->
            ( model, cmd )

        _ ->
            ( model, Err "Unexpected state" |> Ports.sendResult )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Ports.receiveCpuLoadPercent NewCpuLoadPercent


main : Program Flags Model Msg
main =
    Platform.worker
        { init = init
        , update = update
        , subscriptions = subscriptions
        }
