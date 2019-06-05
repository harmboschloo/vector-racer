module VectorRacer.Steps exposing
    ( Steps
    , inSteps
    , steps
    )

import Quantity exposing (Quantity(..))


type Steps
    = Steps


steps : number -> Quantity number Steps
steps =
    Quantity


inSteps : Quantity number Steps -> number
inSteps (Quantity value) =
    value
