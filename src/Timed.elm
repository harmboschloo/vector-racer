module Timed exposing (step, steps)

import Task exposing (Task)
import Time


step : (a -> b) -> a -> Task x ( b, Float )
step fn a =
    Task.map3
        (\startTime b endTime -> ( b, deltaMillis startTime endTime ))
        Time.now
        (Task.succeed () |> Task.map (\_ -> fn a))
        Time.now


steps : List (a -> a) -> a -> Task x ( a, List Float )
steps fns a1 =
    Time.now
        |> Task.andThen
            (\t1 ->
                fns
                    |> List.foldl stepsHelp (Task.succeed ( a1, t1, [] ))
                    |> Task.map
                        (\( a2, _, deltas ) -> ( a2, List.reverse deltas ))
            )


stepsHelp :
    (a -> a)
    -> Task x ( a, Time.Posix, List Float )
    -> Task x ( a, Time.Posix, List Float )
stepsHelp fn task =
    task
        |> Task.andThen
            (\( a1, t1, diffs ) ->
                Task.map2
                    (\a2 t2 -> ( a2, t2, deltaMillis t1 t2 :: diffs ))
                    (Task.succeed () |> Task.map (\_ -> fn a1))
                    Time.now
            )


deltaMillis : Time.Posix -> Time.Posix -> Float
deltaMillis startTime endTime =
    let
        start =
            Time.posixToMillis startTime

        end =
            Time.posixToMillis endTime

        delta =
            toFloat (end - start)
    in
    delta
