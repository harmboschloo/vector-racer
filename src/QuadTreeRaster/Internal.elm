module QuadTreeRaster.Internal exposing
    ( Config
    , Model
    , Node(..)
    , Quads
    , Size
    , get
    , getSize
    , init
    , set
    , withOutOfBoundsValue
    )

import Bitwise


type alias Model a =
    { config : Config a
    , values : Node a
    }


type alias Config a =
    { width : Int
    , height : Int
    , quadSize : Int
    , outOfBoundsValue : a
    }


type Node a
    = Branch (Quads a)
    | Leaf a


type alias Quads a =
    { q1 : Node a
    , q2 : Node a
    , q3 : Node a
    , q4 : Node a
    }


type alias Size =
    { width : Int
    , height : Int
    }


init : Size -> a -> Model a
init size initialValue =
    { config =
        { width = size.width
        , height = size.height
        , quadSize = initQuadSize size
        , outOfBoundsValue = initialValue
        }
    , values = Leaf initialValue
    }


initQuadSize : Size -> Int
initQuadSize size =
    max size.width size.height
        |> toFloat
        |> logBase 2
        |> floor
        |> (^) 2


withOutOfBoundsValue : a -> Model a -> Model a
withOutOfBoundsValue outOfBoundsValue { config, values } =
    { config = { config | outOfBoundsValue = outOfBoundsValue }
    , values = values
    }


getSize : Model a -> Size
getSize { config } =
    { width = config.width
    , height = config.height
    }


set : Int -> Int -> a -> Model a -> Model a
set x y value model =
    if outOfBounds x y model.config then
        model

    else
        { config = model.config
        , values = updateValue x y value model.config.quadSize model.values
        }


get : Int -> Int -> Model a -> a
get x y model =
    if outOfBounds x y model.config then
        model.config.outOfBoundsValue

    else
        getValue x y model.config.quadSize model.values


updateValue : Int -> Int -> a -> Int -> Node a -> Node a
updateValue x y value quadSize node =
    if quadSize == 0 then
        Leaf value

    else
        case node of
            Branch quads ->
                let
                    newNode =
                        updateQuadValue x y value quadSize quads
                in
                case newNode of
                    Branch newQuads ->
                        if
                            (newQuads.q1 == Leaf value)
                                && (newQuads.q2 == Leaf value)
                                && (newQuads.q3 == Leaf value)
                                && (newQuads.q4 == Leaf value)
                        then
                            Leaf value

                        else
                            newNode

                    Leaf _ ->
                        newNode

            Leaf leafValue ->
                if value == leafValue then
                    node

                else
                    updateQuadValue
                        x
                        y
                        value
                        quadSize
                        { q1 = node
                        , q2 = node
                        , q3 = node
                        , q4 = node
                        }


updateQuadValue : Int -> Int -> a -> Int -> Quads a -> Node a
updateQuadValue x y value quadSize quads =
    if x < quadSize then
        if y < quadSize then
            Branch
                { q1 = updateValue x y value (Bitwise.shiftRightBy 1 quadSize) quads.q1
                , q2 = quads.q2
                , q3 = quads.q3
                , q4 = quads.q4
                }

        else
            Branch
                { q1 = quads.q1
                , q2 = quads.q2
                , q3 = updateValue x (y - quadSize) value (Bitwise.shiftRightBy 1 quadSize) quads.q3
                , q4 = quads.q4
                }

    else if y < quadSize then
        Branch
            { q1 = quads.q1
            , q2 = updateValue (x - quadSize) y value (Bitwise.shiftRightBy 1 quadSize) quads.q2
            , q3 = quads.q3
            , q4 = quads.q4
            }

    else
        Branch
            { q1 = quads.q1
            , q2 = quads.q2
            , q3 = quads.q3
            , q4 = updateValue (x - quadSize) (y - quadSize) value (Bitwise.shiftRightBy 1 quadSize) quads.q4
            }


getValue : Int -> Int -> Int -> Node a -> a
getValue x y quadSize node =
    case node of
        Branch quads ->
            if x < quadSize then
                if y < quadSize then
                    getValue x y (Bitwise.shiftRightBy 1 quadSize) quads.q1

                else
                    getValue x (y - quadSize) (Bitwise.shiftRightBy 1 quadSize) quads.q3

            else if y < quadSize then
                getValue (x - quadSize) y (Bitwise.shiftRightBy 1 quadSize) quads.q2

            else
                getValue (x - quadSize) (y - quadSize) (Bitwise.shiftRightBy 1 quadSize) quads.q4

        Leaf value ->
            value


outOfBounds : Int -> Int -> Config a -> Bool
outOfBounds x y config =
    x < 0 || y < 0 || x >= config.width || y >= config.height
