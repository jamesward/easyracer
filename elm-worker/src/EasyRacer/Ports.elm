port module EasyRacer.Ports exposing
    ( ClockTimes
    , FetchResponse
    , receiveCpuUsageResponse
    , receiveFetchResponse
    , sendCpuUsageRequest
    , sendFetchRequest
    , sendResult
    )

--
-- Send scenario results - used by all scenarios
--


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



--
-- Obtain Node CPU load - used by scenario 10
--


type alias ClockTimes =
    { wall : Float
    , user : Float
    , system : Float
    }


port sendCpuUsageRequest : Int -> Cmd msg


port receiveCpuUsageResponse : (ClockTimes -> msg) -> Sub msg



--
-- Use fetch for HTTP request - used by scenario 10
--


type alias FetchResponse =
    { statusCode : Int
    , bodyText : String
    }


port sendFetchRequest : String -> Cmd msg


port receiveFetchResponse : (FetchResponse -> msg) -> Sub msg
