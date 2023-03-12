port module EasyRacer.Scenario1 exposing (main)

import Http
import Platform exposing (Program)


type alias ScenarioResult =
    { isError : Bool
    , value : String
    }


port sendResult : ScenarioResult -> Cmd msg


type alias Flags =
    String


type alias Model =
    ()


type Msg
    = HttpResponse (Result Http.Error String)


init : Flags -> ( Model, Cmd Msg )
init baseUrl =
    ( ()
    , Http.get
        { url = baseUrl
        , expect = Http.expectString HttpResponse
        }
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        HttpResponse (Ok bodyText) ->
            ( model, sendResult { isError = False, value = bodyText } )

        HttpResponse (Err _) ->
            ( model, sendResult { isError = True, value = "HTTP error" } )


main : Program Flags Model Msg
main =
    Platform.worker
        { init = init
        , update = update
        , subscriptions = always Sub.none
        }
