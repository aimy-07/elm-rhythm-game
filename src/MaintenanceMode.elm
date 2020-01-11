port module MaintenanceMode exposing
    ( getMaintenanceMode
    , gotMaintenanceMode
    )


port getMaintenanceMode : () -> Cmd msg


port gotMaintenanceMode : (Maybe Bool -> msg) -> Sub msg
