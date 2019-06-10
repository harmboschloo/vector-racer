module VectorRacer.GridCoordinates exposing
    ( GridCoordinates
    , GridCoordinatesPerStep
    , GridCoordinatesPerStepSquared
    , gridCoordinates
    , gridCoordinatesPerStep
    , gridCoordinatesPerStepSquared
    , inGridCoordinates
    , inGridCoordinatesPerStep
    , inGridCoordinatesPerStepSquared
    )

import Quantity exposing (Quantity(..))
import Quantity.Vector2 as Vector2 exposing (Vector2)
import VectorRacer.Steps exposing (Steps)


type GridCoordinates
    = GridCoordinates


type alias GridCoordinatesPerStep =
    Quantity.Rate GridCoordinates Steps


type alias GridCoordinatesPerStepSquared =
    Quantity.Rate GridCoordinatesPerStep Steps


gridCoordinates : ( number, number ) -> Vector2 number GridCoordinates
gridCoordinates =
    Vector2.fromComponents Quantity


inGridCoordinates : Vector2 number GridCoordinates -> ( number, number )
inGridCoordinates =
    Vector2.toComponents unwrapQuantity


gridCoordinatesPerStep : ( number, number ) -> Vector2 number GridCoordinatesPerStep
gridCoordinatesPerStep =
    Vector2.fromComponents Quantity


inGridCoordinatesPerStep : Vector2 number GridCoordinatesPerStep -> ( number, number )
inGridCoordinatesPerStep =
    Vector2.toComponents unwrapQuantity


gridCoordinatesPerStepSquared : ( number, number ) -> Vector2 number GridCoordinatesPerStepSquared
gridCoordinatesPerStepSquared =
    Vector2.fromComponents Quantity


inGridCoordinatesPerStepSquared : Vector2 number GridCoordinatesPerStepSquared -> ( number, number )
inGridCoordinatesPerStepSquared =
    Vector2.toComponents unwrapQuantity


unwrapQuantity : Quantity number units -> number
unwrapQuantity (Quantity a) =
    a
