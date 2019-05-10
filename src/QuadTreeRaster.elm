module QuadTreeRaster exposing
    ( Raster, Size, init
    , set, get, getSize
    , Token(..), serialize, deserialize
    )

{-|

@docs Raster, Size, init
@docs set, get, getSize
@docs Token, serialize, deserialize

-}

import Bitwise
import QuadTreeRaster.Internal as Internal
    exposing
        ( Model
        , Node(..)
        , Quads
        , Raster(..)
        )


type alias Raster a =
    Internal.Raster a


type alias Size =
    { width : Int
    , height : Int
    }


{-| -}
init : Size -> a -> Raster a
init size initialValue =
    Raster
        { size = initSize size
        , quadSize = initQuadSize size
        , root = LeafNode initialValue
        }


initSize : Size -> Size
initSize size =
    { width = max 0 size.width
    , height = max 0 size.height
    }


initQuadSize : Size -> Int
initQuadSize size =
    max size.width size.height
        |> toFloat
        |> logBase 2
        |> floor
        |> (^) 2


{-| -}
getSize : Raster a -> Size
getSize (Raster { size }) =
    size


outOfBounds : Int -> Int -> Size -> Bool
outOfBounds x y size =
    x < 0 || y < 0 || x >= size.width || y >= size.height


{-| -}
set : Int -> Int -> a -> Raster a -> Raster a
set x y value ((Raster model) as raster) =
    if outOfBounds x y model.size then
        raster

    else
        Raster
            { size = model.size
            , quadSize = model.quadSize
            , root = updateValue x y value model.quadSize model.root
            }


{-| -}
get : Int -> Int -> Raster a -> Maybe a
get x y (Raster model) =
    if outOfBounds x y model.size then
        Nothing

    else
        Just (getValue x y model.quadSize model.root)


{-| FIXME tail-calls
-}
updateValue : Int -> Int -> a -> Int -> Node a -> Node a
updateValue x y value quadSize node =
    if quadSize == 0 then
        LeafNode value

    else
        case node of
            BranchNode quads ->
                let
                    newNode =
                        updateQuadValue x y value quadSize quads
                in
                case newNode of
                    BranchNode newQuads ->
                        if
                            (newQuads.q1 == LeafNode value)
                                && (newQuads.q2 == LeafNode value)
                                && (newQuads.q3 == LeafNode value)
                                && (newQuads.q4 == LeafNode value)
                        then
                            LeafNode value

                        else
                            newNode

                    LeafNode _ ->
                        newNode

            LeafNode leafValue ->
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
            BranchNode
                { q1 = updateValue x y value (Bitwise.shiftRightBy 1 quadSize) quads.q1
                , q2 = quads.q2
                , q3 = quads.q3
                , q4 = quads.q4
                }

        else
            BranchNode
                { q1 = quads.q1
                , q2 = quads.q2
                , q3 = updateValue x (y - quadSize) value (Bitwise.shiftRightBy 1 quadSize) quads.q3
                , q4 = quads.q4
                }

    else if y < quadSize then
        BranchNode
            { q1 = quads.q1
            , q2 = updateValue (x - quadSize) y value (Bitwise.shiftRightBy 1 quadSize) quads.q2
            , q3 = quads.q3
            , q4 = quads.q4
            }

    else
        BranchNode
            { q1 = quads.q1
            , q2 = quads.q2
            , q3 = quads.q3
            , q4 = updateValue (x - quadSize) (y - quadSize) value (Bitwise.shiftRightBy 1 quadSize) quads.q4
            }


getValue : Int -> Int -> Int -> Node a -> a
getValue x y quadSize node =
    case node of
        BranchNode quads ->
            if x < quadSize then
                if y < quadSize then
                    getValue x y (Bitwise.shiftRightBy 1 quadSize) quads.q1

                else
                    getValue x (y - quadSize) (Bitwise.shiftRightBy 1 quadSize) quads.q3

            else if y < quadSize then
                getValue (x - quadSize) y (Bitwise.shiftRightBy 1 quadSize) quads.q2

            else
                getValue (x - quadSize) (y - quadSize) (Bitwise.shiftRightBy 1 quadSize) quads.q4

        LeafNode value ->
            value



-- SERIALIZATION --


{-| -}
type Token a
    = Branch
    | Leaf a


type alias DeserializeHelp a =
    { lastNodes : List (Node a)
    , nextTokens : List (Token a)
    }


{-| -}
serialize : Raster a -> List (Token a)
serialize (Raster { root }) =
    serializeHelp root [] []


serializeHelp : Node a -> List (Node a) -> List (Token a) -> List (Token a)
serializeHelp node nextNodes result =
    case node of
        BranchNode quads ->
            serializeHelp quads.q1 (quads.q2 :: quads.q3 :: quads.q4 :: nextNodes) (Branch :: result)

        LeafNode value ->
            case nextNodes of
                [] ->
                    Leaf value :: result

                nextNode :: otherNextNodes ->
                    serializeHelp nextNode otherNextNodes (Leaf value :: result)


{-| -}
deserialize : Size -> List (Token a) -> Maybe (Raster a)
deserialize size tokens =
    let
        { lastNodes } =
            deserializeHelp
                { lastNodes = []
                , nextTokens = tokens
                }
    in
    case lastNodes of
        root :: [] ->
            Just
                (Raster
                    { size = initSize size
                    , quadSize = initQuadSize size
                    , root = root
                    }
                )

        _ ->
            Nothing


deserializeHelp : DeserializeHelp a -> DeserializeHelp a
deserializeHelp result =
    case result.nextTokens of
        [] ->
            result

        token :: remainingTokens ->
            case token of
                Branch ->
                    case result.lastNodes of
                        q1 :: q2 :: q3 :: q4 :: remainingNodes ->
                            deserializeHelp
                                { lastNodes =
                                    BranchNode
                                        { q1 = q1
                                        , q2 = q2
                                        , q3 = q3
                                        , q4 = q4
                                        }
                                        :: remainingNodes
                                , nextTokens = remainingTokens
                                }

                        _ ->
                            { lastNodes = []
                            , nextTokens = []
                            }

                Leaf value ->
                    deserializeHelp
                        { lastNodes = LeafNode value :: result.lastNodes
                        , nextTokens = remainingTokens
                        }
