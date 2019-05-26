module VectorRacer.Grid exposing
    ( Grid
    , Spacing
    , getAnchorPoint
    , getSpacing
    , init
    )

import Pixels exposing (Pixels)
import VectorRacer exposing (Position)
import VectorRacer.Vector exposing (Vector)


type Grid
    = Grid Model


type alias Model =
    { anchorPoint : Position
    , spacing : Spacing
    }


type alias Spacing =
    Vector Int Pixels


init : { anchorPoint : Position, spacing : Spacing } -> Grid
init config =
    Grid config


getAnchorPoint : Grid -> Position
getAnchorPoint (Grid model) =
    model.anchorPoint


getSpacing : Grid -> Spacing
getSpacing (Grid model) =
    model.spacing
