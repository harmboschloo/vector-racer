module VectorRacer.Grid exposing
    ( Grid
    , Spacing
    , getAnchorPoint
    , getSpacing
    , init
    )

import Pixels exposing (Pixels)
import Quantity.Vector2 exposing (Vector2)
import VectorRacer.Track as Track


type Grid
    = Grid Model


type alias Model =
    { anchorPoint : Track.Position
    , spacing : Spacing
    }


type alias Spacing =
    Vector2 Int Pixels


init : { anchorPoint : Track.Position, spacing : Spacing } -> Grid
init config =
    Grid config


getAnchorPoint : Grid -> Track.Position
getAnchorPoint (Grid model) =
    model.anchorPoint


getSpacing : Grid -> Spacing
getSpacing (Grid model) =
    model.spacing
