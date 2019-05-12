module QuadTreeRaster exposing
    ( Raster, Size, init
    , set, get, getSize
    , foldl, foldr
    , Token(..), serialize, deserialize
    )

{-|

@docs Raster, Size, init
@docs set, get, getSize
@docs foldl, foldr
@docs Token, serialize, deserialize

-}

import QuadTreeRaster.Internal as Internal
    exposing
        ( Model
        , Node(..)
        , Quads
        , Raster(..)
        , initQuadSizes
        )



-- INIT --


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
        , quadSizes = initQuadSizes size
        , root = LeafNode initialValue
        }


initSize : Size -> Size
initSize size =
    { width = max 0 size.width
    , height = max 0 size.height
    }



-- SIZE --


{-| -}
getSize : Raster a -> Size
getSize (Raster { size }) =
    size


outOfBounds : Int -> Int -> Size -> Bool
outOfBounds x y size =
    x < 0 || y < 0 || x >= size.width || y >= size.height



-- SET --


type alias SetHelp a =
    { node : Node a
    , location : Location
    , quadSizes : List Int
    , nodeOperations : List (Node a -> Node a)
    }


type alias Location =
    { x : Int
    , y : Int
    }


{-| -}
set : Int -> Int -> a -> Raster a -> Raster a
set x y value ((Raster model) as raster) =
    if outOfBounds x y model.size then
        raster

    else
        Raster (setValue x y value model)


setValue : Int -> Int -> a -> Model a -> Model a
setValue x y value model =
    { size = model.size
    , quadSizes = model.quadSizes
    , root =
        setHelp
            value
            { location =
                { x = x
                , y = y
                }
            , quadSizes = model.quadSizes
            , node = model.root
            , nodeOperations = []
            }
    }


setHelp : a -> SetHelp a -> Node a
setHelp value { node, location, quadSizes, nodeOperations } =
    case quadSizes of
        [] ->
            List.foldl (<|) (LeafNode value) nodeOperations

        quadSize :: nextQuadSizes ->
            case node of
                BranchNode quads ->
                    setHelp
                        value
                        (updateQuads
                            quads
                            location
                            quadSize
                            nextQuadSizes
                            (mergeQuads value :: nodeOperations)
                        )

                LeafNode leafValue ->
                    if value == leafValue then
                        List.foldl (<|) node nodeOperations

                    else
                        setHelp
                            value
                            (updateQuads
                                { q1 = node
                                , q2 = node
                                , q3 = node
                                , q4 = node
                                }
                                location
                                quadSize
                                nextQuadSizes
                                nodeOperations
                            )


updateQuads : Quads a -> Location -> Int -> List Int -> List (Node a -> Node a) -> SetHelp a
updateQuads quads { x, y } quadSize nextQuadSizes nodeOperations =
    if x < quadSize then
        if y < quadSize then
            { location =
                { x = x
                , y = y
                }
            , quadSizes = nextQuadSizes
            , node = quads.q1
            , nodeOperations =
                (\node ->
                    BranchNode
                        { q1 = node
                        , q2 = quads.q2
                        , q3 = quads.q3
                        , q4 = quads.q4
                        }
                )
                    :: nodeOperations
            }

        else
            { location =
                { x = x
                , y = y - quadSize
                }
            , quadSizes = nextQuadSizes
            , node = quads.q3
            , nodeOperations =
                (\node ->
                    BranchNode
                        { q1 = quads.q1
                        , q2 = quads.q2
                        , q3 = node
                        , q4 = quads.q4
                        }
                )
                    :: nodeOperations
            }

    else if y < quadSize then
        { location =
            { x = x - quadSize
            , y = y
            }
        , quadSizes = nextQuadSizes
        , node = quads.q2
        , nodeOperations =
            (\node ->
                BranchNode
                    { q1 = quads.q1
                    , q2 = node
                    , q3 = quads.q3
                    , q4 = quads.q4
                    }
            )
                :: nodeOperations
        }

    else
        { location =
            { x = x - quadSize
            , y = y - quadSize
            }
        , quadSizes = nextQuadSizes
        , node = quads.q4
        , nodeOperations =
            (\node ->
                BranchNode
                    { q1 = quads.q1
                    , q2 = quads.q2
                    , q3 = quads.q3
                    , q4 = node
                    }
            )
                :: nodeOperations
        }


mergeQuads : a -> Node a -> Node a
mergeQuads value node =
    case node of
        BranchNode quads ->
            let
                mergeNode =
                    LeafNode value
            in
            if
                (quads.q1 == mergeNode)
                    && (quads.q2 == mergeNode)
                    && (quads.q3 == mergeNode)
                    && (quads.q4 == mergeNode)
            then
                mergeNode

            else
                node

        LeafNode _ ->
            node



-- GET --


{-| -}
get : Int -> Int -> Raster a -> Maybe a
get x y (Raster model) =
    if outOfBounds x y model.size then
        Nothing

    else
        getValue x y model.quadSizes model.root


getValue : Int -> Int -> List Int -> Node a -> Maybe a
getValue x y quadSizes node =
    case node of
        BranchNode quads ->
            case quadSizes of
                [] ->
                    Nothing

                quadSize :: nextQuadSizes ->
                    if x < quadSize then
                        if y < quadSize then
                            getValue x y nextQuadSizes quads.q1

                        else
                            getValue x (y - quadSize) nextQuadSizes quads.q3

                    else if y < quadSize then
                        getValue (x - quadSize) y nextQuadSizes quads.q2

                    else
                        getValue (x - quadSize) (y - quadSize) nextQuadSizes quads.q4

        LeafNode value ->
            Just value



-- FOLD --


foldl : (a -> b -> b) -> b -> Raster a -> b
foldl fn acc (Raster model) =
    foldlHelp fn acc model.root []


foldlHelp : (a -> b -> b) -> b -> Node a -> List (Node a) -> b
foldlHelp fn acc node nextNodes =
    case node of
        BranchNode { q1, q2, q3, q4 } ->
            foldlHelp fn acc q1 (q2 :: q3 :: q4 :: nextNodes)

        LeafNode value ->
            case nextNodes of
                [] ->
                    fn value acc

                nextNode :: otherNextNodes ->
                    foldlHelp fn (fn value acc) nextNode otherNextNodes


foldr : (a -> b -> b) -> b -> Raster a -> b
foldr fn acc (Raster model) =
    foldlHelp fn acc model.root []


foldrHelp : (a -> b -> b) -> b -> Node a -> List (Node a) -> b
foldrHelp fn acc node nextNodes =
    case node of
        BranchNode { q1, q2, q3, q4 } ->
            foldrHelp fn acc q4 (q3 :: q2 :: q1 :: nextNodes)

        LeafNode value ->
            case nextNodes of
                [] ->
                    fn value acc

                nextNode :: otherNextNodes ->
                    foldrHelp fn (fn value acc) nextNode otherNextNodes



-- SERIALIZATION --


{-| -}
type Token a
    = Branch
    | Leaf a


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
        ( _, lastNodes ) =
            deserializeHelp ( tokens, [] )
    in
    case lastNodes of
        root :: [] ->
            Just
                (Raster
                    { size = initSize size
                    , quadSizes = initQuadSizes size
                    , root = root
                    }
                )

        _ ->
            Nothing


deserializeHelp : ( List (Token a), List (Node a) ) -> ( List (Token a), List (Node a) )
deserializeHelp ( nextTokens, lastNodes ) =
    case nextTokens of
        [] ->
            ( [], lastNodes )

        token :: remainingTokens ->
            case token of
                Branch ->
                    case lastNodes of
                        q1 :: q2 :: q3 :: q4 :: remainingNodes ->
                            deserializeHelp
                                ( remainingTokens
                                , BranchNode { q1 = q1, q2 = q2, q3 = q3, q4 = q4 } :: remainingNodes
                                )

                        _ ->
                            ( [], [] )

                Leaf value ->
                    deserializeHelp ( remainingTokens, LeafNode value :: lastNodes )
