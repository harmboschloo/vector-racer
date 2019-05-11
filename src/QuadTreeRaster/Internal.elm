module QuadTreeRaster.Internal exposing
    ( Model
    , Node(..)
    , Quads
    , Raster(..)
    , initQuadSizes
    )


type Raster a
    = Raster (Model a)


type alias Model a =
    { size : Size
    , quadSizes : List Int
    , root : Node a
    }


type alias Size =
    { width : Int
    , height : Int
    }


type Node a
    = BranchNode (Quads a)
    | LeafNode a


type alias Quads a =
    { q1 : Node a
    , q2 : Node a
    , q3 : Node a
    , q4 : Node a
    }


initQuadSizes : Size -> List Int
initQuadSizes size =
    if size.width > 0 && size.height > 0 then
        initQuadSizesHelp (max size.width size.height) 1 []

    else
        []


initQuadSizesHelp : Int -> Int -> List Int -> List Int
initQuadSizesHelp size quadSize quadSizes =
    if quadSize < size then
        initQuadSizesHelp size (quadSize * 2) (quadSize :: quadSizes)

    else
        quadSizes
