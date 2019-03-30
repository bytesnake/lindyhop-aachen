module Events.Location exposing (Location, address, locationDecoder, name)

import Json.Decode as Decode
import Json.Encode as Encode


type Location
    = Location
        { name : String
        , address : String
        }


name : Location -> String
name (Location location) =
    location.name


address : Location -> String
address (Location location) =
    location.address


locationDecoder : Decode.Decoder Location
locationDecoder =
    Decode.map2
        (\name_ address_ ->
            Location
                { name = name_
                , address = address_
                }
        )
        (Decode.field "name" Decode.string)
        (Decode.field "address" Decode.string)
