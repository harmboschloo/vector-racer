module QuadTreeRaster.Internal exposing
    ( Model
    , Node(..)
    , Quads
    , Raster(..)
    )


type Raster a
    = Raster (Model a)


type alias Model a =
    { size : Size
    , quadSize : Int
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
