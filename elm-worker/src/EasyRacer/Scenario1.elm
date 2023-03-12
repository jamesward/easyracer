port module EasyRacer.Scenario1 exposing (main)

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
    { result : Maybe String
    , inflightTrackers : Set String
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
    ( { result = Nothing
      , inflightTrackers = Set.fromList requestTrackers
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

                responseResult : Maybe String
                responseResult =
                    Result.toMaybe httpResult

                result : Maybe String
                result =
                    case model.result of
                        Nothing ->
                            responseResult

                        existingResult ->
                            existingResult
            in
            ( { model
                | result = result
                , inflightTrackers = remainingTrackers
              }
            , if not <| Set.isEmpty remainingTrackers then
                case responseResult of
                    Just _ ->
                        remainingTrackers
                            |> Set.toList
                            |> List.map Http.cancel
                            |> Cmd.batch

                    Nothing ->
                        Cmd.none

              else
                result
                    |> Result.fromMaybe "all HTTP requests completed without a single successful response"
                    |> sendResult
            )


main : Program Flags Model Msg
main =
    Platform.worker
        { init = init
        , update = update
        , subscriptions = always Sub.none
        }
