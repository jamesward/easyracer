port module EasyRacer.Scenario2 exposing (main)

import Http
import Platform exposing (Program)
import Set exposing (Set)


type alias ScenarioResult =
    { isError : Bool
    , value : String
    }


port sendResult_ : ScenarioResult -> Cmd msg


type alias Flags =
    String


type alias Model =
    { inflightTrackers : Set String
    }


type Msg
    = HttpResponse String (Result Http.Error String)


init : Flags -> ( Model, Cmd Msg )
init baseUrl =
    let
        requestTrackers =
            [ "first", "second" ]

        httpRequest tracker =
            { method = "GET"
            , headers = []
            , url = baseUrl ++ "/1"
            , body = Http.emptyBody
            , expect = Http.expectString (HttpResponse tracker)
            , timeout = Nothing
            , tracker = Just tracker
            }
    in
    ( { inflightTrackers = Set.fromList requestTrackers
      }
    , requestTrackers
        |> List.map (Http.request << httpRequest)
        |> Cmd.batch
    )


sendResult : Result String String -> Cmd Msg
sendResult result =
    sendResult_ <|
        case result of
            Ok value ->
                { isError = False, value = value }

            Err error ->
                { isError = True, value = error }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        HttpResponse tracker httpResult ->
            let
                remainingTrackers : Set String
                remainingTrackers =
                    model.inflightTrackers |> Set.remove tracker
            in
            ( { model | inflightTrackers = remainingTrackers }
            , case httpResult of
                Ok bodyText ->
                    let
                        cancellations : List (Cmd Msg)
                        cancellations =
                            remainingTrackers
                                |> Set.toList
                                |> List.map Http.cancel

                        returnResult : Cmd Msg
                        returnResult =
                            Ok bodyText |> sendResult
                    in
                    returnResult :: cancellations |> Cmd.batch

                Err _ ->
                    if not <| Set.isEmpty remainingTrackers then
                        Cmd.none

                    else
                        Err "all requests completed without a single success response" |> sendResult
            )


main : Program Flags Model Msg
main =
    Platform.worker
        { init = init
        , update = update
        , subscriptions = always Sub.none
        }
