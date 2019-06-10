module VectorRacer.Grid exposing
    ( Grid
    , Spacing
    , getAnchorPoint
    , getSpacing
    , init
    )

import Pixels exposing (Pixels)
import VectorRacer.Track as Track
import VectorRacer.Vector exposing (Vector)


type Grid
    = Grid Model


type alias Model =
    { anchorPoint : Track.Position
    , spacing : Spacing
    }


type alias Spacing =
    Vector Int Pixels


init : { anchorPoint : Track.Position, spacing : Spacing } -> Grid
init config =
    Grid config


getAnchorPoint : Grid -> Track.Position
getAnchorPoint (Grid model) =
    model.anchorPoint


getSpacing : Grid -> Spacing
getSpacing (Grid model) =
    model.spacing
