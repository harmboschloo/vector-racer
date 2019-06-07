module QuadTreeRaster exposing
    ( Raster, Size, init
    , set, get, getSize
    , foldl, foldr, foldlRegion, foldlLeaves, foldrLeaves
    , Token(..), serialize, deserialize
    )

{-|

@docs Raster, Size, init
@docs set, get, getSize
@docs foldl, foldr, foldlRegion, foldlLeaves, foldrLeaves
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
set : ( Int, Int ) -> a -> Raster a -> Raster a
set ( x, y ) value ((Raster model) as raster) =
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
get : ( Int, Int ) -> Raster a -> Maybe a
get ( x, y ) (Raster model) =
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



-- FOLD POINTS --


foldl : (( Int, Int ) -> a -> b -> b) -> b -> Raster a -> b
foldl fn acc (Raster model) =
    foldlHelp ( 0, 0 ) fn acc model


foldlHelp : ( Int, Int ) -> (( Int, Int ) -> a -> b -> b) -> b -> Model a -> b
foldlHelp ( x, y ) fn acc model =
    if x > model.size.width then
        foldlHelp ( 0, y + 1 ) fn acc model

    else if y > model.size.height then
        acc

    else
        foldlHelp
            ( x + 1, y )
            fn
            (case getValue x y model.quadSizes model.root of
                Just a ->
                    fn ( x, y ) a acc

                Nothing ->
                    acc
            )
            model


foldr : (( Int, Int ) -> a -> b -> b) -> b -> Raster a -> b
foldr fn acc (Raster model) =
    foldrHelp ( model.size.width - 1, model.size.height - 1 ) fn acc model


foldrHelp : ( Int, Int ) -> (( Int, Int ) -> a -> b -> b) -> b -> Model a -> b
foldrHelp ( x, y ) fn acc model =
    if x < 0 then
        foldrHelp ( model.size.width - 1, y - 1 ) fn acc model

    else if y < 0 then
        acc

    else
        foldrHelp
            ( x - 1, y )
            fn
            (case getValue x y model.quadSizes model.root of
                Just a ->
                    fn ( x, y ) a acc

                Nothing ->
                    acc
            )
            model



-- FOLD LEAVES --


{-| TODO: add region to callback (Region -> a -> b -> b)
-}
foldlLeaves : (a -> b -> b) -> b -> Raster a -> b
foldlLeaves fn acc (Raster model) =
    foldlLeavesHelp fn acc model.root []


foldlLeavesHelp : (a -> b -> b) -> b -> Node a -> List (Node a) -> b
foldlLeavesHelp fn acc node nextNodes =
    case node of
        BranchNode { q1, q2, q3, q4 } ->
            foldlLeavesHelp fn acc q1 (q2 :: q3 :: q4 :: nextNodes)

        LeafNode value ->
            case nextNodes of
                [] ->
                    fn value acc

                nextNode :: otherNextNodes ->
                    foldlLeavesHelp fn (fn value acc) nextNode otherNextNodes


{-| TODO: add region to callback (Region -> a -> b -> b)
-}
foldrLeaves : (a -> b -> b) -> b -> Raster a -> b
foldrLeaves fn acc (Raster model) =
    foldrLeavesHelp fn acc model.root []


foldrLeavesHelp : (a -> b -> b) -> b -> Node a -> List (Node a) -> b
foldrLeavesHelp fn acc node nextNodes =
    case node of
        BranchNode { q1, q2, q3, q4 } ->
            foldrLeavesHelp fn acc q4 (q3 :: q2 :: q1 :: nextNodes)

        LeafNode value ->
            case nextNodes of
                [] ->
                    fn value acc

                nextNode :: otherNextNodes ->
                    foldrLeavesHelp fn (fn value acc) nextNode otherNextNodes



-- FOLD REGION --


type alias Region =
    { min : ( Int, Int )
    , max : ( Int, Int )
    }


foldlRegion : Region -> (( Int, Int ) -> Maybe a -> b -> b) -> b -> Raster a -> b
foldlRegion region fn acc raster =
    let
        ( minX, minY ) =
            region.min

        ( maxX, maxY ) =
            region.max
    in
    if minX <= maxX && minY <= maxY then
        foldlRegionHelp ( minX, minY ) region fn acc raster

    else
        acc


foldlRegionHelp : ( Int, Int ) -> Region -> (( Int, Int ) -> Maybe a -> b -> b) -> b -> Raster a -> b
foldlRegionHelp ( x, y ) region fn acc raster =
    let
        ( minX, _ ) =
            region.min

        ( maxX, maxY ) =
            region.max
    in
    if x > maxX then
        foldlRegionHelp ( minX, y + 1 ) region fn acc raster

    else if y > maxY then
        acc

    else
        foldlRegionHelp
            ( x + 1, y )
            region
            fn
            (fn ( x, y ) (get ( x, y ) raster) acc)
            raster



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
