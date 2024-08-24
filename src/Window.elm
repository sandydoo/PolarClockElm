module Window exposing (..)

import Browser.Events as Event


type alias Dimensions =
    { width : Int
    , height : Int
    }


onResize : (Dimensions -> msg) -> Sub msg
onResize func =
    Event.onResize
        (\width height -> func (Dimensions width height))
