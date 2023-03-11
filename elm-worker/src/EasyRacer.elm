port module EasyRacer exposing (main)

import Http
import Platform exposing (Program)


type alias ScenarioNumber =
    Int


type alias ScenarioResult =
    String


port nextScenario : (ScenarioNumber -> msg) -> Sub msg


port sendResult : ScenarioResult -> Cmd msg


type alias Flags =
    { host : String, portNumber : Int }


type alias Model =
    { baseUrl : String }


type Msg
    = ScenarioRequest ScenarioNumber
    | ScenarioResponse (Result Http.Error ScenarioResult)


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { baseUrl = "http://" ++ flags.host ++ ":" ++ String.fromInt flags.portNumber }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ScenarioRequest _ ->
            ( model
            , Http.get
                { url = model.baseUrl
                , expect = Http.expectString ScenarioResponse
                }
            )

        ScenarioResponse (Ok scenarioResult) ->
            ( model, sendResult scenarioResult )

        ScenarioResponse (Err _) ->
            ( model, sendResult "oops" )


subscriptions : Model -> Sub Msg
subscriptions _ =
    nextScenario ScenarioRequest


main : Program Flags Model Msg
main =
    Platform.worker
        { init = init
        , update = update
        , subscriptions = subscriptions
        }
