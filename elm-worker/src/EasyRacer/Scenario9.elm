module EasyRacer.Scenario9 exposing (main)

import EasyRacer.Ports as Ports
import Http
import Platform exposing (Program)
import Set exposing (Set)


type alias Flags =
    String


type alias Model =
    { result : String
    , inflightTrackers : Set String
    }


type Msg
    = HttpResponse String (Result Http.Error String)


scenarioPath : String
scenarioPath =
    "/9"


init : Flags -> ( Model, Cmd Msg )
init baseUrl =
    let
        requestTrackers =
            List.range 1 10
                |> List.map String.fromInt

        httpRequest tracker =
            { method = "GET"
            , headers = []
            , url = baseUrl ++ scenarioPath
            , body = Http.emptyBody
            , expect = Http.expectString (HttpResponse tracker)
            , timeout = Nothing
            , tracker = Just tracker
            }
    in
    ( { result = ""
      , inflightTrackers = Set.fromList requestTrackers
      }
    , requestTrackers
        |> List.map (Http.request << httpRequest)
        |> Cmd.batch
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        HttpResponse tracker httpResult ->
            let
                remainingTrackers : Set String
                remainingTrackers =
                    model.inflightTrackers |> Set.remove tracker

                result : String
                result =
                    model.result ++ (httpResult |> Result.withDefault "")
            in
            ( { model
                | result = result
                , inflightTrackers = remainingTrackers
              }
            , if not <| Set.isEmpty remainingTrackers then
                Cmd.none

              else
                Ok result |> Ports.sendResult
            )


main : Program Flags Model Msg
main =
    Platform.worker
        { init = init
        , update = update
        , subscriptions = always Sub.none
        }
