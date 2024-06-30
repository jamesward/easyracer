port module EasyRacer.Ports exposing (receiveCpuLoadPercent, requestCpuLoadPercent, sendResult)


type alias ScenarioResult =
    { isError : Bool
    , value : String
    }


port sendResult_ : ScenarioResult -> Cmd msg


sendResult : Result String String -> Cmd msg
sendResult result =
    sendResult_ <|
        case result of
            Ok value ->
                { isError = False, value = value }

            Err error ->
                { isError = True, value = error }


port requestCpuLoadPercent : () -> Cmd msg


port receiveCpuLoadPercent : (Float -> msg) -> Sub msg
