module Messages exposing (..)
import Navigation
import Docker.Types exposing (..)

type Msg
    = UrlChange Navigation.Location
    | Receive String
    | PageFilter String
    | ShowImageDialog (Maybe ContainerSpec)
    | AcknowledgeImageDialog

