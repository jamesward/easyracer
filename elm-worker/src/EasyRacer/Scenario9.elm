port module EasyRacer.Scenario9 exposing (main)

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


type alias Msg =
    ()


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( (), sendResult { isError = True, value = "not implemented yet" } )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


main : Program Flags Model Msg
main =
    Platform.worker
        { init = init
        , update = update
        , subscriptions = always Sub.none
        }
