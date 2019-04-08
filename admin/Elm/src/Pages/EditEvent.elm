module Pages.EditEvent exposing (Model)

import Events exposing (Event, Id)


type alias Model =
    { session : Session
    , eventId : Id Event
    , event : Event
    }
