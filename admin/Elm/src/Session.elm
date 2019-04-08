module Session exposing (Session, load)

import Browser.Navigation as Browser
import Task
import Time


type alias Session =
    { timezone : Time.Zone
    }


load : (Session -> msg) -> Cmd msg
load toMsg =
    Task.map (\timezone -> Session timezone) Time.here
        |> Task.perform toMsg
