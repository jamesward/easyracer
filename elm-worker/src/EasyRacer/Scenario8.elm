module EasyRacer.Scenario8 exposing (main)

import EasyRacer.Ports as Ports
import Http
import Platform exposing (Program)
import Set exposing (Set)
import Task


type alias Flags =
    String


type alias Model =
    { baseUrl : String
    , result : Maybe String
    , inflightTrackers : Set String
    }


type Msg
    = OpenHttpResponse String (Result Http.Error String)
    | UseHttpResponse String String (Result Http.Error String)
    | CloseHttpResponse String


scenarioPath : String
scenarioPath =
    "/8"


init : Flags -> ( Model, Cmd Msg )
init baseUrl =
    let
        requestTrackers =
            [ "first", "second" ]

        httpRequest tracker =
            { method = "GET"
            , headers = []
            , url = baseUrl ++ scenarioPath ++ "?open"
            , body = Http.emptyBody
            , expect = Http.expectString (OpenHttpResponse tracker)
            , timeout = Nothing
            , tracker = Just tracker
            }
    in
    ( { baseUrl = baseUrl
      , result = Nothing
      , inflightTrackers = Set.fromList requestTrackers
      }
    , requestTrackers
        |> List.map (Http.request << httpRequest)
        |> Cmd.batch
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OpenHttpResponse tracker httpResult ->
            ( model
            , case httpResult of
                Ok id ->
                    Http.request
                        { method = "GET"
                        , headers = []
                        , url = model.baseUrl ++ scenarioPath ++ "?use=" ++ id
                        , body = Http.emptyBody
                        , expect = Http.expectString (UseHttpResponse tracker id)
                        , timeout = Nothing
                        , tracker = Just tracker
                        }

                Err _ ->
                    Task.succeed tracker |> Task.perform CloseHttpResponse
            )

        UseHttpResponse tracker id httpResult ->
            ( { model | result = Result.toMaybe httpResult }
            , Http.request
                { method = "GET"
                , headers = []
                , url = model.baseUrl ++ scenarioPath ++ "?close=" ++ id
                , body = Http.emptyBody
                , expect = Http.expectWhatever (\_ -> CloseHttpResponse tracker)
                , timeout = Nothing
                , tracker = Just tracker
                }
            )

        CloseHttpResponse tracker ->
            let
                remainingTrackers : Set String
                remainingTrackers =
                    model.inflightTrackers |> Set.remove tracker
            in
            ( { model | inflightTrackers = remainingTrackers }
            , if not <| Set.isEmpty remainingTrackers then
                Cmd.none

              else
                Result.fromMaybe "all requests completed without a single success response" model.result
                    |> Ports.sendResult
            )


main : Program Flags Model Msg
main =
    Platform.worker
        { init = init
        , update = update
        , subscriptions = always Sub.none
        }
