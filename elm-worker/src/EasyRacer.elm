port module EasyRacer exposing (main)

import Http
import Platform exposing (Program)


type alias ScenarioNumber =
    Int


type alias ScenarioResult =
    { isError : Bool
    , value : String
    }


port nextScenario : (ScenarioNumber -> msg) -> Sub msg


port sendResult : ScenarioResult -> Cmd msg


type alias Flags =
    { host : String, portNumber : Int }


type State
    = Idle


type alias Model =
    { baseUrl : String
    , currentState : State
    }


type Msg
    = ExternalRequest ScenarioNumber
    | HttpResponse (Result Http.Error String)


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { baseUrl = "http://" ++ flags.host ++ ":" ++ String.fromInt flags.portNumber
      , currentState = Idle
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ExternalRequest _ ->
            ( model
            , Http.get
                { url = model.baseUrl
                , expect = Http.expectString HttpResponse
                }
            )

        HttpResponse (Ok bodyText) ->
            ( model, sendResult { isError = False, value = bodyText } )

        HttpResponse (Err _) ->
            ( model, sendResult { isError = True, value = "HTTP error" } )


subscriptions : Model -> Sub Msg
subscriptions _ =
    nextScenario ExternalRequest


main : Program Flags Model Msg
main =
    Platform.worker
        { init = init
        , update = update
        , subscriptions = subscriptions
        }
