module VectorRacer.Grid exposing
    ( Grid
    , Spacing
    , getAnchorPoint
    , getSpacing
    , gridToTrackAcceleration
    , gridToTrackPosition
    , gridToTrackVelocity
    , init
    , trackToGridAcceleration
    , trackToGridPosition
    , trackToGridVelocity
    )

import Quantity exposing (Quantity(..))
import Quantity.Vector2 as Vector2 exposing (Vector2)
import VectorRacer.GridCoordinates as GridCoordinates exposing (GridCoordinates)
import VectorRacer.Pixels as Pixels exposing (Pixels)
import VectorRacer.Track as Track


type Grid
    = Grid Model


type alias Model =
    { anchorPoint : Track.Position
    , spacing : Spacing
    }


type alias Spacing =
    Vector2 Int Pixels


type alias Position =
    Vector2 Int GridCoordinates


type alias Velocity =
    Vector2 Int GridCoordinates.GridCoordinatesPerStep


type alias Acceleration =
    Vector2 Int GridCoordinates.GridCoordinatesPerStepSquared


init : { anchorPoint : Track.Position, spacing : Spacing } -> Grid
init config =
    Grid config


getAnchorPoint : Grid -> Track.Position
getAnchorPoint (Grid model) =
    model.anchorPoint


getSpacing : Grid -> Spacing
getSpacing (Grid model) =
    model.spacing



-- CONVERSIONS --


gridToTrackPosition : Grid -> Vector2 Int GridCoordinates -> Vector2 Int Pixels
gridToTrackPosition (Grid { spacing, anchorPoint }) =
    Vector2.map3 gridToTrack spacing anchorPoint


gridToTrackVelocity : Grid -> Vector2 Int GridCoordinates.GridCoordinatesPerStep -> Vector2 Int Pixels.PixelsPerStep
gridToTrackVelocity (Grid { spacing, anchorPoint }) =
    Vector2.map3 gridToTrack spacing anchorPoint


gridToTrackAcceleration :
    Grid
    -> Vector2 Int GridCoordinates.GridCoordinatesPerStepSquared
    -> Vector2 Int Pixels.PixelsPerStepSquared
gridToTrackAcceleration (Grid { spacing, anchorPoint }) =
    Vector2.map3 gridToTrack spacing anchorPoint


gridToTrack : Quantity Int Pixels -> Quantity Int Pixels -> Quantity Int a -> Quantity Int b
gridToTrack (Quantity spacing) (Quantity anchor) (Quantity grid) =
    Quantity
        (grid * spacing + (anchor |> modBy spacing))


trackToGridPosition : Grid -> Vector2 Int Pixels -> Vector2 Float GridCoordinates
trackToGridPosition (Grid { spacing, anchorPoint }) =
    Vector2.map3 trackToGrid spacing anchorPoint


trackToGridVelocity : Grid -> Vector2 Int Pixels.PixelsPerStep -> Vector2 Float GridCoordinates.GridCoordinatesPerStep
trackToGridVelocity (Grid { spacing, anchorPoint }) =
    Vector2.map3 trackToGrid spacing anchorPoint


trackToGridAcceleration :
    Grid
    -> Vector2 Int Pixels.PixelsPerStepSquared
    -> Vector2 Float GridCoordinates.GridCoordinatesPerStepSquared
trackToGridAcceleration (Grid { spacing, anchorPoint }) =
    Vector2.map3 trackToGrid spacing anchorPoint


trackToGrid : Quantity Int Pixels -> Quantity Int Pixels -> Quantity Int a -> Quantity Float b
trackToGrid (Quantity spacing) (Quantity anchor) (Quantity track) =
    Quantity
        (toFloat (track - (anchor |> modBy spacing)) / toFloat spacing)
