module VectorRacer.Grid exposing (Grid, getAnchorPoint, getSpacing, init)

import VectorRacer.Vector exposing (PositionVector)


type Grid
    = Grid Model


type alias Model =
    { anchorPoint : PositionVector
    , spacing : Int
    }


init : PositionVector -> Int -> Grid
init anchorPoint spacing =
    Grid
        { anchorPoint = anchorPoint
        , spacing = spacing
        }


getAnchorPoint : Grid -> PositionVector
getAnchorPoint (Grid model) =
    model.anchorPoint


getSpacing : Grid -> Int
getSpacing (Grid model) =
    model.spacing
