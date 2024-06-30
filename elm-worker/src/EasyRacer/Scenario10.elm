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


type Msg
    = NewId Int
    | NewCpuLoad Float
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
                }
            , Cmd.batch
                [ Task.succeed (BlockingStep ()) |> Task.perform identity
                , Http.get
                    { url = baseUrl ++ scenarioPath ++ "?" ++ String.fromInt id
                    , expect = Http.expectString BlockerHttpResponse
                    }
                , Ports.sendCpuLoadRequest ()
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
        ( Running state, NewCpuLoad load ) ->
            let
                url : String
                url =
                    state.baseUrl
                        ++ scenarioPath
                        ++ "?"
                        ++ String.fromInt state.id
                        ++ "="
                        ++ String.fromFloat load
            in
            ( model
            , Ports.sendFetchRequest url
            )

        ( Running _, ReporterHttpResponse { statusCode, bodyText } ) ->
            ( model
            , case statusCode of
                200 ->
                    Ok bodyText |> Ports.sendResult

                302 ->
                    Process.sleep 1000
                        |> Task.andThen (\_ -> Task.succeed (Perform (Ports.sendCpuLoadRequest ())))
                        |> Task.perform identity

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
        [ Ports.receiveCpuLoadResponse NewCpuLoad
        , Ports.receiveFetchResponse ReporterHttpResponse
        ]


main : Program Flags Model Msg
main =
    Platform.worker
        { init = init
        , update = update
        , subscriptions = subscriptions
        }
