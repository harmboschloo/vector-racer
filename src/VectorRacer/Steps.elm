module VectorRacer.Steps exposing
    ( Steps
    , inSteps
    , steps
    )

import Pixels exposing (Pixels)
import Quantity exposing (Quantity(..))


type Steps
    = Steps


steps : number -> Quantity number Steps
steps =
    Quantity


inSteps : Quantity number Pixels -> number
inSteps (Quantity value) =
    value
