module Timing exposing
    ( time, map, andThen, andTime
    , andThenMap, maybeToTask, resultToTask
    , encode, encodeList
    )

{-|

@docs time, map, andThen, andTime
@docs andThenMap, maybeToTask, resultToTask
@docs encode, encodeList

-}

import Json.Encode
import Task exposing (Task)
import Time



-- TIMING --


time : String -> (() -> a) -> Task e ( a, ( String, Int ) )
time label fn =
    Task.map3
        (\startTime a endTime -> ( a, ( label, deltaMillis startTime endTime ) ))
        Time.now
        (Task.succeed () |> Task.map fn)
        Time.now


map : (a -> b) -> Task e ( a, t ) -> Task e ( b, t )
map fn =
    Task.map (Tuple.mapFirst fn)


andThen :
    (a -> Task e ( b, ( String, Int ) ))
    -> Task e ( a, List ( String, Int ) )
    -> Task e ( b, List ( String, Int ) )
andThen fn =
    Task.andThen
        (\( a, timings ) ->
            fn a |> Task.map (\( b, timing ) -> ( b, timings ++ [ timing ] ))
        )


andTime : String -> (a -> b) -> Task e ( a, List ( String, Int ) ) -> Task e ( b, List ( String, Int ) )
andTime label fn =
    andThen (\a -> time label (\() -> fn a))


deltaMillis : Time.Posix -> Time.Posix -> Int
deltaMillis startTime endTime =
    Time.posixToMillis endTime - Time.posixToMillis startTime



-- TASK HELPERS --


andThenMap : (a -> Task e b) -> Task e ( a, t ) -> Task e ( b, t )
andThenMap fn =
    Task.andThen (\( a, t ) -> fn a |> Task.map (\b -> ( b, t )))


maybeToTask : e -> Maybe a -> Task e a
maybeToTask error maybe =
    case maybe of
        Just a ->
            Task.succeed a

        Nothing ->
            Task.fail error


resultToTask : Result e a -> Task e a
resultToTask result =
    case result of
        Ok a ->
            Task.succeed a

        Err e ->
            Task.fail e



-- JSON HELPERS --


encode : ( String, Int ) -> Json.Encode.Value
encode ( label, millis ) =
    Json.Encode.object
        [ ( "label", Json.Encode.string label )
        , ( "millis", Json.Encode.int millis )
        ]


encodeList : List ( String, Int ) -> Json.Encode.Value
encodeList timings =
    Json.Encode.list encode timings
