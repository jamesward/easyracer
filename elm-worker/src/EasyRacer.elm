module EasyRacer exposing (main)

import Platform exposing (Program)


type alias Flags =
    ()


type alias Model =
    ()


type Msg
    = Msg


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( (), Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions =
    always Sub.none


main : Program Flags Model Msg
main =
    Platform.worker
        { init = init
        , update = update
        , subscriptions = subscriptions
        }

