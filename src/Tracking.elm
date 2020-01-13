port module Tracking exposing
    ( trackingPlayEnd
    , trackingPlayStart
    )


port trackingPlayStart : String -> Cmd msg


port trackingPlayEnd : String -> Cmd msg
