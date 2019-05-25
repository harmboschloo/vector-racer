module VectorRacer.Grid exposing (Grid, getAnchorPoint, getSpacing, init)

import VectorRacer exposing (Position)


type Grid
    = Grid Model


type alias Model =
    { anchorPoint : Position
    , spacing : Int
    }


init : Position -> Int -> Grid
init anchorPoint spacing =
    Grid
        { anchorPoint = anchorPoint
        , spacing = spacing
        }


getAnchorPoint : Grid -> Position
getAnchorPoint (Grid model) =
    model.anchorPoint


getSpacing : Grid -> Int
getSpacing (Grid model) =
    model.spacing
